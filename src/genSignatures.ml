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


open Core.Std

module PS   = GenCommon.PS
module Mode = GenCommon.Mode
module RT   = Runtime

let sprintf = Printf.sprintf
let apply_indent = GenCommon.apply_indent


(* Generate a function for unpacking a capnp union type as an OCaml variant. *)
let generate_union_getter ~nodes_table ~scope ~mode fields =
  match fields with
  | [] ->
      (* If there are no union fields, then suppress the union type *)
      []
  | _ ->
      let indent = String.make (2 * (List.length scope + 2)) ' ' in
      apply_indent ~indent (
        (GenCommon.generate_union_type ~mode nodes_table scope fields) @
          [ "val get : t -> unnamed_union_t" ])


(* Generate accessors for retrieving a selected list of fields of a struct. *)
let generate_getters ~nodes_table ~scope ~mode fields =
  let indent = String.make (2 * (List.length scope + 2)) ' ' in
  List.fold_left fields ~init:[] ~f:(fun acc field ->
    let field_accessors : string list =
      let field_name = String.uncapitalize (PS.Field.R.name_get field) in
      match PS.Field.R.get field with
      | PS.Field.R.Group group ->
          let group_id = PS.Field.Group.R.typeId_get group in
          let group_node = Hashtbl.find_exn nodes_table group_id in
          let group_name =
            GenCommon.get_scope_relative_name nodes_table scope group_node
          in
          let type_name =
            match mode with
            | Mode.Reader  -> "reader_t"
            | Mode.Builder -> "builder_t"
          in [
            sprintf "%sval %s_get : t -> %s.%s"
              indent field_name group_name type_name;
          ]
      | PS.Field.R.Slot slot ->
          let tp = PS.Field.Slot.R.type_get slot in
          let open PS.Type in
          begin match R.get tp with
          | R.Int32 ->
              apply_indent ~indent [
                "val " ^ field_name ^ "_get : t -> int32";
                "val " ^ field_name ^ "_get_int_exn : t -> int";
              ]
          | R.Int64 ->
              apply_indent ~indent [
                "val " ^ field_name ^ "_get : t -> int64";
                "val " ^ field_name ^ "_get_int_exn : t -> int";
              ]
          | R.Uint32 ->
              apply_indent ~indent [
                "val " ^ field_name ^ "_get : t -> Uint32.t";
                "val " ^ field_name ^ "_get_int_exn : t -> int";
              ]
          | R.Uint64 ->
              apply_indent ~indent [
                "val " ^ field_name ^ "_get : t -> Uint64.t";
                "val " ^ field_name ^ "_get_int_exn : t -> int";
              ]
          | R.Void
          | R.Bool
          | R.Int8
          | R.Int16
          | R.Uint8
          | R.Uint16
          | R.Float32
          | R.Float64
          | R.Text
          | R.Data
          | R.List _
          | R.Enum _
          | R.Struct _
          | R.Interface _
          | R.AnyPointer ->
              apply_indent ~indent [
                sprintf "val %s_get : t -> %s"
                  field_name
                  (GenCommon.type_name ~mode ~scope_mode:mode nodes_table scope tp);
              ]
          | R.Undefined_ x ->
              failwith (sprintf "Unknown Type union discriminant %d" x)
          end
      | PS.Field.R.Undefined_ x ->
          failwith (sprintf "Unknown Field union discriminant %d" x)
    in
    (field_accessors @ acc))


(* Generate accessors for setting a selected list of fields of a struct. *)
let generate_setters ~nodes_table ~scope fields =
  let indent = String.make (2 * (List.length scope + 2)) ' ' in
  List.fold_left fields ~init:[] ~f:(fun acc field ->
    let field_accessors : string list =
      let field_name = String.uncapitalize (PS.Field.R.name_get field) in
      match PS.Field.R.get field with
      | PS.Field.R.Group group ->
          []
      | PS.Field.R.Slot slot ->
          let tp = PS.Field.Slot.R.type_get slot in
          let open PS.Type in
          begin match R.get tp with
          | R.Int8 ->
              [ indent ^ "val " ^ field_name ^ "_set_exn : t -> int -> unit"; ]
          | R.Int16 ->
              [ indent ^ "val " ^ field_name ^ "_set_exn : t -> int -> unit"; ]
          | R.Int32 ->
              apply_indent ~indent [
                "val " ^ field_name ^ "_set : t -> int32 -> unit";
                "val " ^ field_name ^ "_set_int_exn : t -> int -> unit";
              ]
          | R.Int64 ->
              apply_indent ~indent [
                "val " ^ field_name ^ "_set : t -> int64 -> unit";
                "val " ^ field_name ^ "_set_int_exn : t -> int -> unit";
              ]
          | R.Uint8 ->
              [ indent ^ "val " ^ field_name ^ "_set_exn : t -> int -> unit"; ]
          | R.Uint16 ->
              [ indent ^ "val " ^ field_name ^ "_set_exn : t -> int -> unit"; ]
          | R.Uint32 ->
              apply_indent ~indent [
                "val " ^ field_name ^ "_set : t -> Uint32.t -> unit";
                "val " ^ field_name ^ "_set_int_exn : t -> int -> unit";
              ]
          | R.Uint64 ->
              apply_indent ~indent [
                "val " ^ field_name ^ "_set : t -> Uint64.t -> unit";
                "val " ^ field_name ^ "_set_int_exn : t -> int -> unit";
              ]
          | R.List _ ->
              apply_indent ~indent [
                (* FIXME: should allow setting from a Reader *)
                sprintf "val %s_set : t -> %s -> %s"
                  field_name
                  (GenCommon.type_name ~mode:Mode.Builder ~scope_mode:Mode.Builder
                     nodes_table scope tp)
                  (GenCommon.type_name ~mode:Mode.Builder ~scope_mode:Mode.Builder
                     nodes_table scope tp);
                sprintf "val %s_init : t -> int -> %s"
                  field_name
                  (GenCommon.type_name ~mode:Mode.Builder ~scope_mode:Mode.Builder
                     nodes_table scope tp);
              ]
          | R.Struct _ ->
              apply_indent ~indent [
                sprintf "val %s_set_reader : t -> %s -> %s"
                  field_name
                  (GenCommon.type_name ~mode:Mode.Reader ~scope_mode:Mode.Builder
                     nodes_table scope tp)
                  (GenCommon.type_name ~mode:Mode.Builder ~scope_mode:Mode.Builder
                     nodes_table scope tp);
                sprintf "val %s_set_builder : t -> %s -> %s"
                  field_name
                  (GenCommon.type_name ~mode:Mode.Builder ~scope_mode:Mode.Builder
                     nodes_table scope tp)
                  (GenCommon.type_name ~mode:Mode.Builder ~scope_mode:Mode.Builder
                     nodes_table scope tp);
                sprintf "val %s_init : t -> %s"
                  field_name
                  (GenCommon.type_name ~mode:Mode.Builder ~scope_mode:Mode.Builder
                     nodes_table scope tp);
              ]
          | R.Enum _ ->
              apply_indent ~indent [
                sprintf "val %s_set : t -> %s -> unit"
                  field_name
                  (GenCommon.type_name ~mode:Mode.Builder ~scope_mode:Mode.Builder
                     nodes_table scope tp);
                sprintf "val %s_set_unsafe : t -> %s -> unit"
                  field_name
                  (GenCommon.type_name ~mode:Mode.Builder ~scope_mode:Mode.Builder
                     nodes_table scope tp);
              ]
          | R.Bool
          | R.Float32
          | R.Float64
          | R.Text
          | R.Data
          | R.Interface _
          | R.AnyPointer ->
              apply_indent ~indent [
                sprintf "val %s_set : t -> %s -> unit"
                field_name
                (GenCommon.type_name ~mode:Mode.Builder ~scope_mode:Mode.Builder
                   nodes_table scope tp);
              ]
          | R.Void ->
              (* For void types, we suppress the argument *)
              [ indent ^ "val " ^ field_name ^ "_set : t -> unit" ]
          | R.Undefined_ x ->
              failwith (sprintf "Unknown Type union discriminant %d" x)
          end
      | PS.Field.R.Undefined_ x ->
          failwith (sprintf "Unknown Field union discriminant %d" x)
    in
    (field_accessors @ acc))


(* Generate the OCaml type signature corresponding to a struct definition.  [scope] is a
 * stack of scope IDs corresponding to this lexical context, and is used to figure
 * out what module prefixes are required to properly qualify a type.
 *
 * Raises: Failure if the children of this node contain a cycle. *)
let rec generate_struct_node ~nodes_table ~scope ~nested_modules
    ~node struct_def : string list =
  let unsorted_fields =
    RT.Array.to_list (PS.Node.Struct.R.fields_get struct_def)
  in
  (* Sorting in reverse code order allows us to avoid a List.rev *)
  let all_fields = List.sort unsorted_fields ~cmp:(fun x y ->
    - (Int.compare (PS.Field.R.codeOrder_get x) (PS.Field.R.codeOrder_get y)))
  in
  let union_fields, non_union_fields = List.partition_tf all_fields
      ~f:(fun field ->
        (PS.Field.R.discriminantValue_get field) <> PS.Field.noDiscriminant)
  in
  let reader_union_accessors =
    generate_union_getter ~nodes_table ~scope ~mode:Mode.Reader union_fields
  in
  let reader_non_union_accessors =
    generate_getters ~nodes_table ~scope ~mode:Mode.Reader non_union_fields
  in
  let builder_union_accessors =
    (generate_union_getter ~nodes_table ~scope ~mode:Mode.Builder union_fields) @
      (generate_setters ~nodes_table ~scope union_fields)
  in
  let builder_non_union_accessors =
    (generate_getters ~nodes_table ~scope ~mode:Mode.Builder non_union_fields) @
      (generate_setters ~nodes_table ~scope non_union_fields)
  in
  let indent = String.make (2 * (List.length scope + 1)) ' ' in
  let unique_reader =
    GenCommon.make_unique_typename ~mode:Mode.Reader ~nodes_table node
  in
  let unique_builder =
    GenCommon.make_unique_typename ~mode:Mode.Builder ~nodes_table node
  in
  let header = apply_indent ~indent [
    (* declare the primary types of the node *)
    "type reader_t";
    "type builder_t";
    (* declare schema-unique type aliases for these types, for use in
       submodules *)
    "type " ^ unique_reader ^ " = reader_t";
    "type " ^ unique_builder ^ " = builder_t";
  ] in
  let reader = [
    indent ^ "module R : sig";
    indent ^ "  type t = reader_t"; ] @
      reader_union_accessors @
      reader_non_union_accessors @ [
      indent ^ "  val of_message : 'cap message_t -> t";
      indent ^ "end";
    ]
  in
  let builder = [
    indent ^ "module B : sig";
    indent ^ "  type t = builder_t"; ] @
      builder_union_accessors @
      builder_non_union_accessors @ [
      indent ^ "  val of_message : rw message_t -> t";
      indent ^ "end";
    ]
  in
  header @
    nested_modules @
    reader @
    builder


(* Generate the OCaml type signature corresponding to a node.  [scope] is
 * a stack of scope IDs corresponding to this lexical context, and is used to figure out
 * what module prefixes are required to properly qualify a type.
 *
 * Raises: Failure if the children of this node contain a cycle. *)
and generate_node
    ~(suppress_module_wrapper : bool)
    ~(nodes_table : (Uint64.t, PS.Node.reader_t) Hashtbl.t)
    ~(scope : Uint64.t list)
    ~(node_name : string)
    (node : PS.Node.reader_t)
: string list =
  let open PS.Node in
  let node_id = R.id_get node in
  let indent = String.make (2 * (List.length scope)) ' ' in
  let generate_nested_modules () =
    match Topsort.topological_sort nodes_table
            (GenCommon.children_of nodes_table node) with
    | Some child_nodes ->
        List.concat_map child_nodes ~f:(fun child ->
          let child_name = GenCommon.get_unqualified_name ~parent:node ~child in
          let child_node_id = R.id_get child in
          generate_node ~suppress_module_wrapper:false ~nodes_table
            ~scope:(child_node_id :: scope) ~node_name:child_name child)
    | None ->
        let error_msg = sprintf
          "The children of node %s (%s) have a cyclic dependency."
          (Uint64.to_string node_id)
          (R.displayName_get node)
        in
        failwith error_msg
  in
  match R.get node with
  | R.File ->
      generate_nested_modules ()
  | R.Struct struct_def ->
      let nested_modules = generate_nested_modules () in
      let body =
        generate_struct_node ~nodes_table ~scope ~nested_modules ~node struct_def
      in
      if suppress_module_wrapper then
        body
      else
        [ indent ^ "module " ^ node_name ^ " : sig" ] @
          body @
          [ indent ^ "end" ]
  | R.Enum enum_def ->
      let nested_modules = generate_nested_modules () in
      let body =
        GenCommon.generate_enum_sig ~nodes_table ~scope ~nested_modules
          (* FIXME: hack *)
          ~mode:Mode.Reader
          ~node enum_def
      in
      if suppress_module_wrapper then
        body
      else
        [ indent ^ "module " ^ node_name ^ " : sig" ] @
          body @
          [ indent ^ "end" ]
  | R.Interface iface_def ->
      generate_nested_modules ()
  | R.Const const_def -> [
      sprintf "%sval %s : %s"
        indent
        (String.uncapitalize node_name)
        (GenCommon.type_name ~mode:Mode.Reader ~scope_mode:Mode.Reader
           nodes_table scope (Const.R.type_get const_def));
    ]
  | R.Annotation annot_def ->
      generate_nested_modules ()
  | R.Undefined_ x ->
      failwith (sprintf "Unknown Node union discriminant %u" x)



