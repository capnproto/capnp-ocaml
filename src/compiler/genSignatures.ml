(******************************************************************************
 * capnp-ocaml
 *
 * Copyright (c) 2013-2014, Paul Pelzl
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimer.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 ******************************************************************************)


open Core_kernel.Std

module PS      = GenCommon.PS
module Context = GenCommon.Context
module Mode    = GenCommon.Mode
module C = Capnp

let sprintf = Printf.sprintf
let failf msg = Format.kasprintf failwith msg
let apply_indent = GenCommon.apply_indent

(* Generate a function for unpacking a capnp union type as an OCaml variant. *)
let generate_union_getter ~context ~scope ~mode fields =
  match fields with
  | [] ->
      (* If there are no union fields, then suppress the union type *)
      []
  | _ ->
      (GenCommon.generate_union_type ~context ~mode scope fields) @
        [ "val get : t -> unnamed_union_t" ]


type accessor_t =
  | Getter of string list
  | Setter of string list


let generate_one_field_accessors ~context ~scope ~mode field
  : accessor_t list =
  let field_name = GenCommon.underscore_name (PS.Field.name_get field) in
  match PS.Field.get field with
  | PS.Field.Group group ->
      let group_id = PS.Field.Group.type_id_get group in
      let group_node = Hashtbl.find_exn context.Context.nodes group_id in
      let group_name =
        GenCommon.get_scope_relative_name ~context scope group_node
      in [
        Getter [
          sprintf "val %s_get : t -> %s.t"
            field_name group_name;
        ];
        Setter [
          sprintf "val %s_init : t -> %s.t"
            field_name group_name
        ];
      ]
  | PS.Field.Slot slot ->
      let tp = PS.Field.Slot.type_get slot in
      let open PS.Type in
      begin match get tp with
      | Int8
      | Int16
      | Uint8
      | Uint16 -> [
          Getter [ "val " ^ field_name ^ "_get : t -> int"; ];
          Setter [ "val " ^ field_name ^ "_set_exn : t -> int -> unit"; ];
        ]
      | Int32 -> [
          Getter ["val " ^ field_name ^ "_get : t -> int32"; ];
          Getter ["val " ^ field_name ^ "_get_int_exn : t -> int"; ];
          Setter [ "val " ^ field_name ^ "_set : t -> int32 -> unit"; ];
          Setter [ "val " ^ field_name ^ "_set_int_exn : t -> int -> unit"; ]
        ]
      | Int64 -> [
          Getter [ "val " ^ field_name ^ "_get : t -> int64"; ];
          Getter [ "val " ^ field_name ^ "_get_int_exn : t -> int"; ];
          Setter [ "val " ^ field_name ^ "_set : t -> int64 -> unit"; ];
          Setter [ "val " ^ field_name ^ "_set_int : t -> int -> unit"; ];
        ]
      | Uint32 -> [
          Getter [ "val " ^ field_name ^ "_get : t -> Uint32.t"; ];
          Getter [ "val " ^ field_name ^ "_get_int_exn : t -> int"; ];
          Setter [ "val " ^ field_name ^ "_set : t -> Uint32.t -> unit"; ];
          Setter [ "val " ^ field_name ^ "_set_int_exn : t -> int -> unit"; ]
        ]
      | Uint64 -> [
          Getter [ "val " ^ field_name ^ "_get : t -> Uint64.t"; ];
          Getter [ "val " ^ field_name ^ "_get_int_exn : t -> int"; ];
          Setter [ "val " ^ field_name ^ "_set : t -> Uint64.t -> unit"; ];
          Setter [ "val " ^ field_name ^ "_set_int_exn : t -> int -> unit"; ];
        ]
      | Void -> [
          Getter [ "val " ^ field_name ^ "_get : t -> unit"; ];
          (* For void types, we suppress extra the setter argument *)
          Setter [ "val " ^ field_name ^ "_set : t -> unit"; ];
        ]
      | Bool
      | Float32
      | Float64 -> [
          Getter [
            sprintf "val %s_get : t -> %s"
              field_name
              (GenCommon.type_name ~context ~mode ~scope_mode:mode scope tp); ];
          Setter [
            sprintf "val %s_set : t -> %s -> unit"
              field_name
              (GenCommon.type_name ~context ~mode ~scope_mode:mode scope tp); ];
        ]
      | Text
      | Data -> [
          Getter [
            "val has_" ^ field_name ^ " : t -> bool"; ];
          Getter [
            sprintf "val %s_get : t -> %s"
              field_name
              (GenCommon.type_name ~context ~mode ~scope_mode:mode scope tp); ];
          Setter [
            sprintf "val %s_set : t -> %s -> unit"
              field_name
              (GenCommon.type_name ~context ~mode ~scope_mode:mode scope tp); ];
        ]
      | Interface _ -> [
          Getter [
            "val " ^ field_name ^ "_get : t -> Uint32.t option"; ];
          Setter [
            "val " ^ field_name ^ "_set : t -> Uint32.t option -> unit"; ];
        ]
      | AnyPointer -> [
          Getter [
            sprintf "val %s_get : t -> %s"
              field_name
              (GenCommon.type_name ~context ~mode ~scope_mode:mode scope tp);
            sprintf "val %s_get_interface : t -> Uint32.t option"
              field_name ];
          Setter [
            sprintf "val %s_set : t -> %s -> %s"
              field_name
              (GenCommon.type_name ~context ~mode ~scope_mode:mode scope tp)
              (GenCommon.type_name ~context ~mode ~scope_mode:mode scope tp);
            sprintf "val %s_set_reader : t -> %s -> %s"
              field_name
              (GenCommon.type_name ~context ~mode:Mode.Reader ~scope_mode:mode scope tp)
              (GenCommon.type_name ~context ~mode ~scope_mode:mode scope tp);
            sprintf "val %s_set_interface : t -> Uint32.t option -> unit"
              field_name ];
        ]
      | List list_descr ->
          let list_type = List.element_type_get list_descr in [
          Getter [
            "val has_" ^ field_name ^ " : t -> bool"; ];
          Getter [
            sprintf "val %s_get : t -> %s"
              field_name
              (GenCommon.type_name ~context ~mode ~scope_mode:mode scope tp); ];
          Getter [
            sprintf "val %s_get_list : t -> %s list"
              field_name
              (GenCommon.type_name ~context ~mode ~scope_mode:mode scope list_type); ];
          Getter [
            sprintf "val %s_get_array : t -> %s array"
              field_name
              (GenCommon.type_name ~context ~mode ~scope_mode:mode scope list_type); ];
          Setter [
            (* FIXME: should allow setting from a Reader *)
            sprintf "val %s_set : t -> %s -> %s"
              field_name
              (GenCommon.type_name ~context ~mode ~scope_mode:mode scope tp)
              (GenCommon.type_name ~context ~mode ~scope_mode:mode scope tp); ];
          Setter [
            sprintf "val %s_set_list : t -> %s list -> %s"
              field_name
              (GenCommon.type_name ~context ~mode ~scope_mode:mode scope list_type)
              (GenCommon.type_name ~context ~mode ~scope_mode:mode scope tp); ];
          Setter [
            sprintf "val %s_set_array : t -> %s array -> %s"
              field_name
              (GenCommon.type_name ~context ~mode ~scope_mode:mode scope list_type)
              (GenCommon.type_name ~context ~mode ~scope_mode:mode scope tp); ];
          Setter [
            sprintf "val %s_init : t -> int -> %s"
              field_name
              (GenCommon.type_name ~context ~mode ~scope_mode:mode scope tp); ];
        ]
      | Enum _ -> [
          Getter [
            sprintf "val %s_get : t -> %s"
              field_name
              (GenCommon.type_name ~context ~mode ~scope_mode:mode scope tp); ];
          Setter [
            sprintf "val %s_set : t -> %s -> unit"
              field_name
              (GenCommon.type_name ~context ~mode ~scope_mode:mode scope tp); ];
          Setter [
            sprintf "val %s_set_unsafe : t -> %s -> unit"
              field_name
              (GenCommon.type_name ~context ~mode ~scope_mode:mode scope tp); ];
        ]
      | Struct _ -> [
          Getter [
            "val has_" ^ field_name ^ " : t -> bool"; ];
          Getter [
            sprintf "val %s_get : t -> %s"
              field_name
              (GenCommon.type_name ~context ~mode ~scope_mode:mode scope tp); ];
          Setter [
            sprintf "val %s_set_reader : t -> %s -> %s"
              field_name
              (GenCommon.type_name ~context ~mode:Mode.Reader
                 ~scope_mode:Mode.Builder scope tp)
              (GenCommon.type_name ~context ~mode:Mode.Builder
                 ~scope_mode:Mode.Builder scope tp); ];
          Setter [
            sprintf "val %s_set_builder : t -> %s -> %s"
              field_name
              (GenCommon.type_name ~context ~mode:Mode.Builder
                 ~scope_mode:Mode.Builder scope tp)
              (GenCommon.type_name ~context ~mode:Mode.Builder
                 ~scope_mode:Mode.Builder scope tp); ];
          Setter [
            sprintf "val %s_init : t -> %s"
              field_name
              (GenCommon.type_name ~context ~mode:Mode.Builder
                 ~scope_mode:Mode.Builder scope tp); ];
        ]
      | Undefined x ->
          failwith (sprintf "Unknown Type union discriminant %d" x)
      end
  | PS.Field.Undefined x ->
      failwith (sprintf "Unknown Field union discriminant %d" x)


(* Generate accessors for getting or setting the selected fields.  The specified
   filter function allows the caller to restrict the result to getter or setter
   functions. *)
let generate_accessors ~context ~scope ~mode
    ~(f : accessor_t -> bool) fields
  : string list =
  List.fold_left fields ~init:[] ~f:(fun acc field ->
    let accessors = generate_one_field_accessors ~context ~scope ~mode field in
    let filtered_accessors = List.fold_left accessors ~init:[]
        ~f:(fun acc accessor ->
          if f accessor then
            match accessor with
            | Getter x
            | Setter x -> acc @ x
          else
            acc)
    in
    filtered_accessors @ acc)


(* Generate the OCaml type signature corresponding to a struct definition.  [scope] is a
 * stack of scope IDs corresponding to this lexical context, and is used to figure
 * out what module prefixes are required to properly qualify a type.
 *
 * For struct without parents (e.g. auto-generated method parameters), [uqname] must be
 * supplied.
 *
 * Raises: Failure if the children of this node contain a cycle. *)
let generate_struct_node ?uq_name ~context ~scope ~nested_modules
    ~mode ~node struct_def : string list =
  let unsorted_fields =
    C.Array.to_list (PS.Node.Struct.fields_get struct_def)
  in
  (* Sorting in reverse code order allows us to avoid a List.rev *)
  let all_fields = List.sort unsorted_fields ~cmp:(fun x y ->
    - (Int.compare (PS.Field.code_order_get x) (PS.Field.code_order_get y)))
  in
  let union_fields, non_union_fields = List.partition_tf all_fields
      ~f:(fun field ->
        (PS.Field.discriminant_value_get field) <> PS.Field.no_discriminant)
  in
  let union_accessors =
    (generate_union_getter ~context ~scope ~mode union_fields) @
       begin match mode with
       | Mode.Reader ->
           []
       | Mode.Builder ->
           generate_accessors ~context ~scope ~mode
             ~f:(function Getter _ -> false | Setter _ -> true) union_fields
       end
  in
  let non_union_accessors =
    let selector =
      match mode with
      | Mode.Reader  -> (function Getter _ -> true | Setter _ -> false)
      | Mode.Builder -> (fun x -> true)
    in
    generate_accessors ~context ~scope ~mode ~f:selector non_union_fields
  in
  let unique_reader = GenCommon.make_unique_typename ?uq_name ~mode:Mode.Reader
      ~context node
  in
  let unique_builder = GenCommon.make_unique_typename ?uq_name ~mode:Mode.Builder
      ~context node
  in
  let header =
    match mode with
    | Mode.Reader -> [
        "type t = " ^ unique_reader;
        "type builder_t = " ^ unique_builder;
      ]
    | Mode.Builder -> [
        "type t = " ^ unique_builder;
        "type reader_t = " ^ unique_reader;
      ]
  in
  let footer =
    match mode with
    | Mode.Reader -> [
        "val of_message : 'cap message_t -> t";
        "val of_builder : builder_t -> t";
        "val of_pointer : pointer_t -> t";
      ]
    | Mode.Builder -> [
        "val of_message : rw message_t -> t";
        "val to_message : t -> rw message_t";
        "val to_reader : t -> reader_t";
        "val init_root : ?message_size:int -> unit -> t";
        "val init_pointer : pointer_t -> t";
      ]
  in
  header @
    nested_modules @
    union_accessors @
    non_union_accessors @
    footer

let generate_methods ~context ~scope ~nested_modules ~mode interface_def : string list =
  let module Method = GenCommon.Method in
  let methods = Method.methods_of_interface ~context interface_def in
  (* todo: superclasses *)
  let make_auto m phase =
    match Method.auto_struct phase m with
    | `Existing _ -> []
    | `Auto (struct_node, struct_def, mod_name) ->
      let uq_name = Method.capnp_name m in
      let body = generate_struct_node
          ~uq_name ~context ~scope ~nested_modules:[] ~mode ~node:struct_node struct_def
      in
      [ "module " ^ mod_name ^ " : sig" ] @
      (apply_indent ~indent:"  " body) @
      [ "end" ]
  in
  let structs =
    List.map methods ~f:(fun m ->
        make_auto m Method.Params @
        make_auto m Method.Results
      )
    |> List.concat
  in
  match mode with
  | Mode.Reader ->
    let client =
      let methods =
        List.map methods ~f:(fun m ->
            let params = Method.(payload_type Params) ~mode m in
            let results = Method.(payload_type Results) ~mode m in
            sprintf "method %s : (%s, %s) proxy_method_t" (Method.ocaml_name m) params results
          )
      in
      [ "class client : rpc_client_t -> object" ] @
      (apply_indent ~indent:"  " methods) @
      [ "end" ]
    in
    nested_modules @ structs @ client
  | Mode.Builder ->
    let server =
      let body =
        List.map methods ~f:(fun m ->
            sprintf "method %s : (%s, %s) method_impl_t"
              (Method.ocaml_name m)
              (Method.(payload_type Params) ~mode m)
              (Method.(payload_type Results) ~mode m)
          )
      in
      [ "class type server = object" ] @
      (apply_indent ~indent:"  " body) @
      [ "end";
        "val dispatch : #server -> interface_id:Uint64.t -> method_id:int -> generic_method_t";
      ]
    in
    nested_modules @ structs @ server

(* Generate the OCaml type signature corresponding to a node.  [scope] is
 * a stack of scope IDs corresponding to this lexical context, and is used to figure out
 * what module prefixes are required to properly qualify a type.
 *
 * Raises: Failure if the children of this node contain a cycle. *)
let rec generate_node
    ~(suppress_module_wrapper : bool)
    ~(context : Context.codegen_context_t)
    ~(scope : Uint64.t list)
    ~(mode : Mode.t)
    ~(node_name : string)
    (node : PS.Node.t)
: string list =
  let open PS.Node in
  let node_id = id_get node in
  let generate_nested_modules () =
    match Topsort.topological_sort context.Context.nodes
            (GenCommon.children_of ~context node) with
    | Some child_nodes ->
        List.concat_map child_nodes ~f:(fun child ->
          let child_name = GenCommon.get_unqualified_name ~parent:node ~child in
          let child_node_id = id_get child in
          generate_node ~suppress_module_wrapper:false ~context
            ~scope:(child_node_id :: scope) ~mode ~node_name:child_name child)
    | None ->
        let error_msg = sprintf
          "The children of node %s (%s) have a cyclic dependency."
          (Uint64.to_string node_id)
          (display_name_get node)
        in
        failwith error_msg
  in
  match get node with
  | File ->
      generate_nested_modules ()
  | Struct struct_def ->
      let nested_modules = generate_nested_modules () in
      let body =
        generate_struct_node ~context ~scope ~nested_modules ~mode 
          ~node struct_def
      in
      if suppress_module_wrapper then
        body
      else
        [ "module " ^ node_name ^ " : sig" ] @
          (apply_indent ~indent:"  " body) @
          [ "end" ]
  | Enum enum_def ->
      let unique_module_name =
        (String.capitalize node_name) ^ "_" ^ (Uint64.to_string node_id)
      in
      let body =
        (generate_nested_modules ()) @
        (GenCommon.generate_enum_sig ~unique_module_name enum_def)
      in
      if suppress_module_wrapper then
        body
      else
        [ "module " ^ node_name ^ " : sig" ] @
          (apply_indent ~indent:"  " body) @
          [ "end" ]
  | Interface iface_def ->
      let nested_modules = generate_nested_modules () in
      let unique_reader = GenCommon.make_unique_typename ~context ~mode:Mode.Reader node in
      let body = [
        "type t = " ^ unique_reader;
        "type reader_t = " ^ unique_reader;
        "val interface_id : Uint64.t";
      ] @ generate_methods ~context ~scope ~nested_modules ~mode iface_def
      in
      if suppress_module_wrapper then
        body
      else
        [ "module " ^ node_name ^ " : sig" ] @
          (apply_indent ~indent:"  " body) @
          [ "end" ]
  | Const const_def -> [
      sprintf "val %s : %s"
        (GenCommon.underscore_name node_name)
        (GenCommon.type_name ~context ~mode:Mode.Reader ~scope_mode:mode
           scope (Const.type_get const_def));
    ]
  | Annotation annot_def ->
      generate_nested_modules ()
  | Undefined x ->
      failwith (sprintf "Unknown Node union discriminant %u" x)



