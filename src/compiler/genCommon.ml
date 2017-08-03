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

module M   = Capnp.BytesMessage
module PS_ = PluginSchema.Make(M)
module PS  = PS_.Reader
module C   = Capnp

let sprintf = Printf.sprintf
let failf msg = Format.ksprintf failwith msg
let uint64_equal = C.Runtime.Util.uint64_equal


module Context = struct
  type import_t = {
    (* ID associated with an imported file *)
    id : Uint64.t;

    (* Name which the current module uses to refer to the import *)
    schema_name : string;

    (* Actual module name associated with the import *)
    module_name : string;
  }

  type codegen_context_t = {
    (* Table of all nodes found in the code generation request *)
    nodes : (Uint64.t, PS.Node.t) Hashtbl.t;

    (* Position in output order. *)
    positions : (Uint64.t, int) Hashtbl.t;

    (* List of imports associated with the module to be generated *)
    imports : import_t list;
  }

  let node t id =
    match Hashtbl.find t.nodes id with
    | None -> failf "Node with ID %s not found" (Uint64.to_string id)
    | Some x -> x

  let position t id =
    Hashtbl.find t.positions id
end


(* Modes in which code generation can be run *)
module Mode = struct
  type t =
    | Reader
    | Builder

  let flip = function
    | Reader -> Builder
    | Builder -> Reader
end


let apply_indent ~(indent : string) (lines : string list) : string list =
  List.map lines ~f:(fun line ->
    if String.is_empty line then "" else indent ^ line)


(* Mangle a name so that it doesn't collide with any of the names in the list. *)
let mangle_ident (ident : string) (idents : string list) =
  let rec loop mangled =
    if List.mem ~equal:String.equal idents mangled then
      loop (mangled ^ "_")
    else
      mangled
  in
  loop ident

let mangle_undefined reserved_names =
  String.capitalize (mangle_ident "undefined" reserved_names)

let mangle_enum_undefined (enumerants : ('a, 'b, 'c) C.Array.t) =
  let enumerant_names = C.Array.map_list enumerants ~f:PS.Enumerant.name_get in
  mangle_undefined enumerant_names

let mangle_field_undefined (fields : 'a list) =
  let field_names = List.rev_map fields ~f:PS.Field.name_get in
  mangle_undefined field_names


(* Module filenames are alphanumeric and start with uppercase alpha.  Cap'n Proto
   schema filenames have very weakly restricted naming, so we have to perform a
   transformation on illegal characters. *)
let make_legal_module_name schema_filename =
  let base = Filename.chop_extension (Filename.basename schema_filename) in
  let candidate =
    String.concat_map base ~f:(fun c ->
      if Char.is_alphanum c || c = '_' then
        String.make 1 c
      else if c = '-' then
        String.make 1 '_'
      else
        sprintf "%02x" (Char.to_int c))
  in
  if Char.is_alpha candidate.[0] then
    String.capitalize candidate
  else
    "M" ^ candidate


let underscore_name (camelcase_name : string) : string =
  let rev_chars : string list =
    String.fold (String.uncapitalize camelcase_name) ~init:[] ~f:(fun acc c ->
      let fragment =
        if Char.is_uppercase c then
          "_" ^ (String.of_char (Char.lowercase c))
        else
          String.of_char c
      in
      fragment :: acc)
  in
  String.concat ~sep:"" (List.rev rev_chars)


let child_ids_of
    (parent : PS.Node.t)
  : Uint64.t list =
  let nested_nodes =
    PS.Node.nested_nodes_get_list parent
    |> List.map ~f:PS.Node.NestedNode.id_get
  in
  match PS.Node.get parent with
  | PS.Node.Struct s ->
    let fields = PS.Node.Struct.fields_get_list s in
    let groups = List.filter_map fields ~f:(fun field ->
        match PS.Field.get field with
        | PS.Field.Group g -> Some (PS.Field.Group.type_id_get g)
        | _ -> None
      )
    in
    groups @ nested_nodes
  | _ -> nested_nodes

let children_of
    ~(context : Context.codegen_context_t)
    (parent : PS.Node.t)
: PS.Node.t list =
  let node_ids = child_ids_of parent in
  List.map node_ids ~f:(Context.node context)

(* The name of a node is not encoded in that node, it is encoded in the parent.
 * So we have to implement a little search logic to get a programmatic name for
 * a node.
 *
 * Raises: Failure if we can't find the node in its parent.  (This means that capnpc
 * emitted a schema that we don't fully understand...) *)
let get_unqualified_name
    ~(parent : PS.Node.t)
    ~(child  : PS.Node.t)
: string =
  let open PS.Node in
  let child_id = id_get child in
  let nested_nodes = nested_nodes_get parent in
  let matching_nested_node_name =
    C.Array.find_map nested_nodes ~f:(fun nested_node ->
      if uint64_equal child_id (NestedNode.id_get nested_node) then
        Some (NestedNode.name_get nested_node)
      else
        None)
  in
  match matching_nested_node_name with
  | Some s ->
      String.capitalize s
  | None ->
      let error_msg = sprintf
          "Unable to find unqualified name of child node %s (%s) \
           within parent node %s (%s)."
        (Uint64.to_string child_id)
        (display_name_get child)
        (Uint64.to_string (id_get parent))
        (display_name_get parent)
      in
      begin match get parent with
      | File
      | Enum _
      | Interface _
      | Const _
      | Annotation _ ->
          failwith error_msg
      | Struct node_struct ->
          let fields = Struct.fields_get node_struct in
          let matching_field_name =
            C.Array.find_map fields ~f:(fun field ->
              match PS.Field.get field with
              | PS.Field.Slot _ ->
                  None
              | PS.Field.Group group ->
                  if uint64_equal child_id
                      (PS.Field.Group.type_id_get group) then
                    Some (String.capitalize (PS.Field.name_get field))
                  else
                    None
              | PS.Field.Undefined x ->
                  failwith (sprintf "Unknown Field union discriminant %d" x))
          in
          begin match matching_field_name with
          | Some name -> name
          | None      -> failwith error_msg
          end
      | PS.Node.Undefined x ->
          failwith (sprintf "Unknown Node union discriminant %d" x)
      end


(* Get a representation of the fully-qualified module name for [node].
 * The resulting list associates each component of the name with the scope
 * which it defines.  The head of the list is at the outermost scope. *)
let get_fully_qualified_name_components ~context node
  : (string * Uint64.t) list =
  let open PS.Node in
  let rec loop acc curr_node =
    let scope_id = scope_id_get curr_node in
    if uint64_equal scope_id Uint64.zero then
      acc
    else
      let parent = Context.node context scope_id in
      let node_name = get_unqualified_name ~parent ~child:curr_node in
      let node_id = id_get curr_node in
      loop ((node_name, node_id) :: acc) parent
  in
  loop [] node


(* Get the fully-qualified name for [node]. *)
let get_fully_qualified_name ~context node : string =
  get_fully_qualified_name_components ~context node |>
  List.map ~f:fst |>
  String.concat ~sep:"."


(* Get a qualified module name for [node] which is suitable for use at the given
 * [scope_stack] position. *)
let get_scope_relative_name ~context (scope_stack : Uint64.t list) node
  : string =
  let rec pop_components components scope =
    match components, scope with
    | ( (_component_name, component_scope_id) ::
          other_components, scope_id :: scope_ids) ->
        if uint64_equal component_scope_id scope_id then
          pop_components other_components scope_ids
        else
          components
    | _ ->
        components
  in
  let fq_name = get_fully_qualified_name_components ~context node in
  let rel_name = pop_components fq_name (List.rev scope_stack) in
  String.concat ~sep:"." (List.map rel_name ~f:fst)


let make_unique_enum_module_name ~context enum_node =
  let uq_name = get_unqualified_name
      ~parent:(Context.node context (PS.Node.scope_id_get enum_node))
      ~child:enum_node
  in
  let node_id = PS.Node.id_get enum_node in
  (String.capitalize uq_name) ^ "_" ^ (Uint64.to_string node_id)


let make_unique_typename ?uq_name ?mode ~context node =
  match PS.Node.get node with
  | PS.Node.Enum _ ->
      (* Enums don't have unique type names, they have unique module names.  This
         allows us to use the same enum constructor names without name collisions. *)
      let unique_module_name = make_unique_enum_module_name ~context node in
      unique_module_name ^ ".t"
  | PS.Node.Interface _ ->
      (* Interfaces don't have separate builder types. *)
      let uq_name =
        get_unqualified_name
          ~parent:(Context.node context (PS.Node.scope_id_get node))
          ~child:node
      in
      let node_id = PS.Node.id_get node in
      sprintf "[`%s_%Lx]" uq_name (Uint64.to_int64 node_id)
  | _ ->
      let opt_mode =
        match mode with
        | None -> ""
        | Some Mode.Reader -> " reader_t"
        | Some Mode.Builder -> " builder_t"
      in
      let uq_name =
        match uq_name with
        | Some x -> x
        | None -> get_unqualified_name
                    ~parent:(Context.node context (PS.Node.scope_id_get node))
                    ~child:node
      in
      let node_id = PS.Node.id_get node in
      sprintf "[`%s_%Lx]%s" uq_name (Uint64.to_int64 node_id) opt_mode


(* Determines whether a simple name for [node], as given by
 * [get_scope_relative_name], may be ambiguous for referring to the [node]
 * from the given [scope]. *)
let is_node_naming_collision ~context ~scope node =
  let target_name = get_scope_relative_name ~context scope node in
  let target_scope_id = PS.Node.scope_id_get node in
  List.fold_left scope ~init:false
    ~f:(fun is_collision scope_id ->
      if is_collision then
        true
      else if uint64_equal target_scope_id scope_id then
        (* Skipping over the target node *)
        false
      else
        Hashtbl.fold context.Context.nodes ~init:false
          ~f:(fun ~key:_ ~data:other_node found ->
            if found then
              true
            else
              if uint64_equal scope_id (PS.Node.scope_id_get other_node) then
                let candidate_name =
                  get_scope_relative_name ~context scope other_node
                in
                String.equal target_name candidate_name
              else
                false))


(* Find the import which provides the specified node, if any. *)
let find_import_providing_node ~context node : Context.import_t option =
  let rec loop_node_scope n =
    let scope_id = PS.Node.scope_id_get n in
    if scope_id = Uint64.zero then
      None
    else
      match List.find_map context.Context.imports ~f:(fun import ->
          if uint64_equal import.Context.id scope_id then
            Some import
          else
            None) with
      | Some import ->
          Some import
      | None ->
          let parent = Context.node context scope_id in
          loop_node_scope parent
  in
  loop_node_scope node


let rec union_imports_for_struct_fields ~visited_node_ids ~import_ids ~context
    (struct_descr : PS.Node.Struct.t) : unit =
  let struct_fields = PS.Node.Struct.fields_get struct_descr in
  C.Array.iter struct_fields ~f:(fun field ->
    match PS.Field.get field with
    | PS.Field.Slot slot ->
        let contained_type = PS.Field.Slot.type_get slot in
        union_imports_for_type ~visited_node_ids ~import_ids ~context contained_type
    | PS.Field.Group group ->
        let contained_type_id = PS.Field.Group.type_id_get group in
        let contained_type_node =
          Context.node context contained_type_id
        in
        begin match find_import_providing_node ~context contained_type_node with
        | Some import -> Hashtbl.set import_ids ~key:import.Context.id ~data:()
        | None -> ()
        end
    | PS.Field.Undefined x ->
        failwith (sprintf "Unknown Type union discriminant %d" x))


(* Determine the set of imports necessary to define the given type, and union
   them with the [import_ids] hash table. *)
and union_imports_for_type ~visited_node_ids ~import_ids ~context (tp : PS.Type.t) : unit =
  let open PS.Type in
  match get tp with
  | Void
  | Bool
  | Int8
  | Int16
  | Int32
  | Int64
  | Uint8
  | Uint16
  | Uint32
  | Uint64
  | Float32
  | Float64
  | Text
  | Data
  | Enum _
  | AnyPointer _ -> ()
  | List list_descr ->
      let contained_type = List.element_type_get list_descr in
      union_imports_for_type ~visited_node_ids ~import_ids ~context contained_type
  | Struct struct_descr ->
      (* It's possible for a struct to contain recursively-defined types.
         So track nodes already visited in order to ensure termination. *)
      let struct_id = Struct.type_id_get struct_descr in
      begin match Hashtbl.add visited_node_ids ~key:struct_id ~data:() with
      | `Ok ->
          let struct_node = Context.node context struct_id in
          let () = begin match PS.Node.get struct_node with
          | PS.Node.Struct struct_descr ->
              union_imports_for_struct_fields ~visited_node_ids ~import_ids
                ~context struct_descr
          | _ ->
              failwith "found non-struct node where struct node was expected"
          end in
          begin match find_import_providing_node ~context struct_node with
          | Some import -> Hashtbl.set import_ids ~key:import.Context.id ~data:()
          | None -> ()
          end
      | `Duplicate ->
          ()
      end
  | Interface iface_descr ->
      (* FIXME: flesh this out when interfaces are properly supported *)
      let iface_id = Interface.type_id_get iface_descr in
      let iface_node = Context.node context iface_id in
      begin match find_import_providing_node ~context iface_node with
      | Some import -> Hashtbl.set import_ids ~key:import.Context.id ~data:()
      | None -> ()
      end
  | Undefined x ->
      failwith (sprintf "Unknown Type union discriminant %d" x)


(** Not all imports contain interesting content.  In particular, the
    common import "/capnp/c++.capnp" contains nothing of OCaml relevance.
    [find_interesting_import_ids] recurses through the node tree to
    extract the subset of imports which contain interesting content. *)
and find_interesting_import_ids ~visited_node_ids ~import_ids ~context node =
  (* It's possible for a struct to contain recursively-defined types.
     So track nodes already visited in order to ensure termination. *)
  let node_id = PS.Node.id_get node in
  match Hashtbl.add visited_node_ids ~key:node_id ~data:() with
  | `Duplicate ->
      ()
  | `Ok ->
      let () = List.iter (children_of ~context node)
          ~f:(fun child -> find_interesting_import_ids ~visited_node_ids
              ~import_ids ~context child)
      in
      begin match PS.Node.get node with
      | PS.Node.Const const_descr ->
          let tp = PS.Node.Const.type_get const_descr in
          union_imports_for_type ~visited_node_ids ~import_ids ~context tp
      | PS.Node.Struct struct_descr ->
          let () = union_imports_for_struct_fields ~visited_node_ids ~import_ids
              ~context struct_descr
          in
          begin match find_import_providing_node ~context node with
          | Some import -> Hashtbl.set import_ids ~key:import.Context.id ~data:()
          | None -> ()
          end
      | PS.Node.Enum _
      | PS.Node.File
      | PS.Node.Annotation _
      | PS.Node.Interface _ ->
          (* FIXME: recurse here when interfaces are complete *)
          begin match find_import_providing_node ~context node with
          | Some import -> Hashtbl.set import_ids ~key:import.Context.id ~data:()
          | None -> ()
          end
      | PS.Node.Undefined x ->
          failwith (sprintf "Unknown Node union discriminant %u" x)
      end


let filter_interesting_imports ~context root_node : Context.codegen_context_t =
  let interesting_ids = Hashtbl.Poly.create () in
  let visited_node_ids = Hashtbl.Poly.create () in
  let () = find_interesting_import_ids ~visited_node_ids ~import_ids:interesting_ids
      ~context root_node
  in
  let filtered_imports = List.filter context.Context.imports ~f:(fun import ->
      Hashtbl.mem interesting_ids import.Context.id)
  in
  { context with Context.imports = filtered_imports }


(* When modules refer to types defined in other modules, readability dictates
 * that we use OtherModule.t/OtherModule.reader_t/OtherModule.builder_t as
 * the preferred type name.  However, consider the case of nested modules:
 *
 * module Foo : sig
 *   type t
 *   type builder_t
 *   type t_Foo_UID = reader_t
 *   type builder_t_Foo_UID = builder_t
 *
 *   module Bar : sig
 *     type t
 *     type builder_t
 *     type t_Bar_UID = reader_t
 *     type builder_t_Bar_UID = builder_t
 *
 *     val foo_get : t -> t_Foo_UID
 *   end
 *   ...
 * end
 *
 * In this case, module Foo does not have a complete declaration at the time
 * foo_get is declared, so we can't refer to the type as "Foo.t".  So for
 * this case instead of using Foo.t we emit an unambiguous type identifier
 * based on the 64-bit unique ID for Foo.
 *
 * A second case occurs when two different nodes with the same name are
 * visible at the same scope:
 *
 * struct Foo {
 *   struct Nested {
 *     ...
 *   }
 *
 *   struct Bar {
 *     struct Nested {
 *       ...
 *     }
 *
 *     nested @0 :Foo.Nested;
 *   }
 * }
 *
 * In this case we must ensure that the type of field 'nested' is
 * emitted in an unambiguous form which cannot be confused with Foo.Bar.Nested.
 * The test here is "at this scope, are there multiple nodes present with the
 * same name as the target node"? *)
let make_disambiguated_type_name ~context ~(mode : Mode.t) ~(scope_mode : Mode.t)
    ~scope ~tp node =
  let node_id = PS.Node.id_get node in
  let scope_position =
    match scope with
    | [] -> 0
    | x :: _ ->
      match Context.position context x with
      | None -> assert false
      | Some x -> x
  in
  let target_position =
    match Context.position context node_id with
    | Some y -> y
    | None -> -1   (* Import *)
  in
  if target_position > scope_position then
    (* The target is defined later in the file. Emit an unambiguous type. *)
    make_unique_typename ~context ~mode node
  else if List.mem ~equal:uint64_equal scope node_id then
    (* The node of interest is a parent node of the node being generated.
       this is a case where an unambiguous type is emitted. *)
    make_unique_typename ~context ~mode node
  else if is_node_naming_collision ~context ~scope node then
    (* A scope-relative name would be ambiguous due to the presence of
       another node with the same name.  Emit an unambiguous type. *)
    make_unique_typename ~context ~mode node
  else
    match find_import_providing_node ~context node with
    | Some import ->
        (* This type comes from an import.  Emit a unique typename qualified
           with the proper import. *)
        let uq_name = make_unique_typename ~context ~mode node in
        begin match PS.Node.get node with
        | PS.Node.Struct _ | PS.Node.Interface _ -> uq_name     (* Polymorphic variant type *)
        | _ -> import.Context.schema_name ^ "." ^ uq_name
        end
    | None ->
        let module_name = get_scope_relative_name ~context scope node in
        let t_str =
          match tp with
          | `Enum | `Interface ->
              (* Enum and interface types are identical across reader and builder, no need
                 to distinguish between them *)
              ".t"
          | `Struct ->
              begin match (mode, scope_mode) with
              | (Mode.Reader, Mode.Reader)
              | (Mode.Builder, Mode.Builder) ->
                  ".t"
              | (Mode.Reader, Mode.Builder) ->
                  ".struct_t reader_t"
              | (Mode.Builder, Mode.Reader) ->
                  ".struct_t builder_t"
              end
        in
        module_name ^ t_str


(* Construct an ocaml name for the given schema-defined type.
   [mode] indicates whether the generated type name represents a Reader or a
   Builder type.  [scope_mode] indicates whether the generated type name is
   to be referenced within the scope of a Reader or a Builder. *)
let rec type_name ~context ~(mode : Mode.t) ~(scope_mode : Mode.t)
    scope tp : string =
  let open PS.Type in
  match get tp with
  | Void    -> "unit"
  | Bool    -> "bool"
  | Int8    -> "int"
  | Int16   -> "int"
  | Int32   -> "int32"
  | Int64   -> "int64"
  | Uint8   -> "int"
  | Uint16  -> "int"
  | Uint32  -> "Uint32.t"
  | Uint64  -> "Uint64.t"
  | Float32 -> "float"
  | Float64 -> "float"
  | Text    -> "string"
  | Data    -> "string"
  | List list_descr ->
      let list_type = List.element_type_get list_descr in
      sprintf "(%s, %s, %s) Capnp.Array.t"
        (if mode = Mode.Reader then "ro" else "rw")
        (type_name ~context ~mode ~scope_mode scope list_type)
        begin match (mode, scope_mode) with
        | (Mode.Reader, Mode.Reader)
        | (Mode.Builder, Mode.Builder) ->
            "array_t"
        | (Mode.Reader, Mode.Builder) ->
            "reader_array_t"
        | (Mode.Builder, Mode.Reader) ->
            "builder_array_t"
        end
  | Enum enum_descr ->
      let enum_id = Enum.type_id_get enum_descr in
      let enum_node = Context.node context enum_id in
      make_disambiguated_type_name ~context ~mode ~scope_mode
        ~scope ~tp:`Enum enum_node
  | Struct struct_descr ->
      let struct_id = Struct.type_id_get struct_descr in
      let struct_node = Context.node context struct_id in
      make_disambiguated_type_name ~context ~mode ~scope_mode
        ~scope ~tp:`Struct struct_node
  | Interface iface_descr ->
      let iface_id = Interface.type_id_get iface_descr in
      let iface_node = Context.node context iface_id in
      make_disambiguated_type_name ~context ~mode ~scope_mode
        ~scope ~tp:`Interface iface_node
  | AnyPointer _ ->
      begin match (mode, scope_mode) with
      | (Mode.Reader, Mode.Reader)
      | (Mode.Builder, Mode.Builder) ->
          "pointer_t"
      | (Mode.Reader, Mode.Builder) ->
          "Reader.pointer_t"
      | (Mode.Builder, Mode.Reader) ->
          "Builder.pointer_t"
      end
  | Undefined x ->
      failwith (sprintf "Unknown Type union discriminant %d" x)


let generate_union_type ~context ~(mode : Mode.t) scope fields =
  let open PS.Field in
  let cases = List.fold_left fields ~init:[] ~f:(fun acc field ->
    let field_name = String.capitalize (name_get field) in
    match get field with
    | Slot slot ->
        let field_type = Slot.type_get slot in
        begin match PS.Type.get field_type with
        | PS.Type.Void ->
            ("  | " ^ field_name) :: acc
        | PS.Type.Interface _ ->
            (sprintf "  | %s of %s RPC.Capability.t option"
             field_name
             (type_name ~context ~mode ~scope_mode:mode scope field_type))
            :: acc
        | _ ->
            ("  | " ^ field_name ^ " of " ^
               (type_name ~context ~mode ~scope_mode:mode scope field_type))
            :: acc
        end
    | Group group ->
        let group_type_name =
          let group_id = Group.type_id_get group in
          let group_node = Context.node context group_id in
          let group_module_name =
            get_scope_relative_name ~context scope group_node
          in
          group_module_name ^ ".t"
        in
        ("  | " ^ field_name ^ " of " ^ group_type_name) :: acc
    | Undefined x ->
        failwith (sprintf "Unknown Field union discriminant %d" x))
  in
  let undefined_name = mangle_field_undefined fields in
  let header = [ "type unnamed_union_t =" ] in
  let footer = [ sprintf "  | %s of int" (String.capitalize undefined_name) ] in
  (header @ cases @ footer)


(* Generate a decoder function for converting from a uint16 to the
   associated enum value. *)
let generate_enum_decoder enum_def =
  let header = [ "let decode u16 = match u16 with" ] in
  let enumerants = PS.Node.Enum.enumerants_get enum_def in
  let match_cases =
    C.Array.foldi_right enumerants ~init:[] ~f:(fun i enumerant acc ->
      let case_str =
        sprintf "  | %u -> %s" i
          (String.capitalize (PS.Enumerant.name_get enumerant))
      in
      case_str :: acc)
  in
  let undefined_name = mangle_enum_undefined enumerants in
  let footer = [
    sprintf "  | v -> %s v" undefined_name
  ] in
  header @ match_cases @ footer


(* Generate an encoder function for converting from an enum value to the
   associated uint16. *)
let generate_enum_encoder ~unsafe enum_def =
  let header =
    if unsafe then
      [ "let encode_unsafe enum = match enum with" ]
    else
      [ "let encode_safe enum = match enum with" ]
  in
  let enumerants = PS.Node.Enum.enumerants_get enum_def in
  let match_cases =
    C.Array.foldi_right enumerants ~init:[] ~f:(fun i enumerant acc ->
      let case_str =
        sprintf "  | %s -> %u"
          (String.capitalize (PS.Enumerant.name_get enumerant))
          i
      in
      case_str :: acc)
  in
  let footer =
    let undefined_name = mangle_enum_undefined enumerants in
    if unsafe then
      [ sprintf "  | %s x -> x" undefined_name ]
    else
      [ sprintf "  | %s x -> \
                 invalid_msg \"Cannot encode undefined enum value.\""
          undefined_name ]
  in
  header @ match_cases @ footer


(* Generate the signature for an enum type. *)
let generate_enum_sig ?unique_module_name enum_def =
  let header =
    match unique_module_name with
    | Some name ->
        [ "type t = " ^ name ^ ".t =" ]
    | None ->
        [ "type t =" ]
  in
  let variants =
    let enumerants = PS.Node.Enum.enumerants_get enum_def in
    let undefined_name = mangle_enum_undefined enumerants in
    let footer = [
      sprintf "  | %s of int" (String.capitalize undefined_name)
    ] in
    C.Array.fold_right enumerants ~init:footer ~f:(fun enumerant acc ->
      let name = String.capitalize (PS.Enumerant.name_get enumerant) in
      let match_case = "  | " ^ name in
      match_case :: acc)
  in
  header @ variants


let method_param_types ~method_name:uq_name ~context node =
  let struct_name =
    let node_id = PS.Node.id_get node in
    sprintf "struct_%s_%s" uq_name (Uint64.to_string node_id)
  in
  let reader_name = make_unique_typename ~uq_name ~context ~mode:Mode.Reader node in
  let builder_name = make_unique_typename ~uq_name ~context ~mode:Mode.Builder node in
  [
    (builder_name, `Public (struct_name ^ " builder_t"));
    (reader_name, `Public (struct_name ^ " reader_t"));
    (struct_name, `Abstract);
  ]


let method_types ~context interface_def =
  let methods = PS.Node.Interface.methods_get_list interface_def in
  List.map methods ~f:(fun method_def ->
      let method_name = PS.Method.name_get method_def in
      let make_auto struct_id =
        let struct_node = Context.node context struct_id in
        if PS.Node.scope_id_get struct_node = Uint64.zero then (
          match PS.Node.get struct_node with
          | PS.Node.Struct _ ->
            method_param_types ~method_name ~context struct_node
          | _ ->
            failf "Method payload %s is not a struct!" (PS.Node.display_name_get struct_node)
        ) else []
      in
      let params = make_auto @@ PS.Method.param_struct_type_get method_def in
      let result = make_auto @@ PS.Method.result_struct_type_get method_def in
      params @ result
    )
  |> List.concat


(* Recurse through the schema, emitting uniquely-named modules for
   all enum types. *)
let rec collect_unique_enums ?(toplevel = true) ~is_sig ~context node =
  let child_decls = List.concat_map (children_of ~context node)
      ~f:(fun child_node ->
        collect_unique_enums ~toplevel:false ~is_sig ~context child_node)
  in
  let parent_decl =
    match PS.Node.get node with
    | PS.Node.File
    | PS.Node.Const _
    | PS.Node.Annotation _
    | PS.Node.Struct _
    | PS.Node.Interface _ ->
        []
    | PS.Node.Enum enum_def ->
        let unique_module_name = make_unique_enum_module_name ~context node in
        let body = generate_enum_sig enum_def in
        let codecs =
          if is_sig then
            []
          else
            (* Emit encoder and decoder functions for private use within the
               module implementation *)
            (generate_enum_decoder enum_def) @
              (generate_enum_encoder ~unsafe:false enum_def) @
              (generate_enum_encoder ~unsafe:true enum_def)
        in
        let header =
          if is_sig then
            [ "module " ^ unique_module_name ^ " : sig" ]
          else
            [ "module " ^ unique_module_name ^ " = struct" ]
        in
        header @ (apply_indent ~indent:"  " (body @ codecs)) @ [ "end" ]
    | PS.Node.Undefined x ->
        failwith (sprintf "Unknown Node union discriminant %u" x)
  in
  let all_decls = (List.rev_append parent_decl child_decls) in
  if toplevel then
    (* Toplevel call *)
    List.rev all_decls
  else
    (* Recursive call *)
    all_decls


module Method = struct
  type phase = Params | Results

  type t = {
    method_id : int;
    method_def : PS.Method.t;
    context : Context.codegen_context_t;
  }

  let create ~context method_id method_def =
    { method_id; method_def; context }

  let capnp_name t = PS.Method.name_get t.method_def

  let ocaml_name t = underscore_name (capnp_name t)

  let payload t = function
    | Params -> PS.Method.param_struct_type_get t.method_def
    | Results -> PS.Method.result_struct_type_get t.method_def

  let auto_struct phase t =
    let struct_id = payload t phase in
    let struct_node = Context.node t.context struct_id in
    match PS.Node.get struct_node with
    | PS.Node.Struct struct_def ->
      (* If scopeID is zero then the struct was auto-generated, and we should emit it. *)
      if PS.Node.scope_id_get struct_node = Uint64.zero then (
        let prefix = String.capitalize (capnp_name t) in
        let mod_name =
          match phase with
          | Params -> prefix ^ "_params"
          | Results -> prefix ^ "_results"
        in
        `Auto (struct_node, struct_def, mod_name)
      ) else `Existing struct_node
    | _ -> failf "Method payload %s is not a struct!" (PS.Node.display_name_get struct_node)

  let payload_type ~context ~mode:scope_mode ~scope phase t =
    match auto_struct phase t with
    | `Existing struct_node ->
      let mode = match phase with Params -> Mode.flip scope_mode | Results -> scope_mode in
      make_disambiguated_type_name ~context ~mode ~scope_mode ~scope ~tp:`Struct struct_node
    | `Auto (_, _, mod_name) ->
      match scope_mode, phase with
      | Mode.Reader, Params -> mod_name ^ ".struct_t builder_t"
      | Mode.Reader, Results -> mod_name ^ ".t"
      | Mode.Builder, Params -> mod_name ^ ".struct_t reader_t"
      | Mode.Builder, Results -> mod_name ^ ".t"

  let id t = t.method_id

  let methods_of_interface ~context interface_def =
    PS.Node.Interface.methods_get_list interface_def |> List.mapi ~f:(create ~context)
end


