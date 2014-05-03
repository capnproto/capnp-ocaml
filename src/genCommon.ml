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

module M   = Message.Make(StrStorage)
module PS_ = PluginSchema.Make(M)
module PS  = PS_.Reader
module RT  = Runtime

let sprintf = Printf.sprintf

(* Modes in which code generation can be run *)
module Mode = struct
  type t =
    | Reader
    | Builder
end


let apply_indent ~(indent : string) (lines : string list) : string list =
  List.map lines ~f:(fun line ->
    if String.is_empty line then "" else indent ^ line)


(* Mangle a name so that it doesn't collide with any of the names in the list. *)
let mangle_ident (ident : string) (idents : string list) =
  let rec loop mangled =
    if List.mem idents mangled then
      loop (mangled ^ "_")
    else
      mangled
  in
  loop ident


let mangle_undefined reserved_names =
  String.capitalize (mangle_ident "undefined" reserved_names)

let mangle_enum_undefined (enumerants : ('a, 'b, 'c) RT.Array.t) =
  let enumerant_names = RT.Array.map_list enumerants ~f:PS.Enumerant.name_get in
  mangle_undefined enumerant_names

let mangle_field_undefined (fields : 'a list) =
  let field_names = List.rev_map fields ~f:PS.Field.name_get in
  mangle_undefined field_names


let children_of
    (nodes_table : (Uint64.t, PS.Node.t) Hashtbl.t)
    (parent : PS.Node.t)
: PS.Node.t list =
  let open PS.Node in
  let parent_id = id_get parent in
  Hashtbl.fold nodes_table ~init:[] ~f:(fun ~key:id ~data:node acc ->
    if Util.uint64_equal parent_id (scopeId_get node) then
      node :: acc
    else
      acc)


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
  let nested_nodes = nestedNodes_get parent in
  let matching_nested_node_name =
    RT.Array.find_map nested_nodes ~f:(fun nested_node ->
      if Util.uint64_equal child_id (NestedNode.id_get nested_node) then
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
        (displayName_get child)
        (Uint64.to_string (id_get parent))
        (displayName_get parent)
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
            RT.Array.find_map fields ~f:(fun field ->
              match PS.Field.get field with
              | PS.Field.Slot _ ->
                  None
              | PS.Field.Group group ->
                  if Util.uint64_equal child_id
                      (PS.Field.Group.typeId_get group) then
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
let get_fully_qualified_name_components nodes_table node
  : (string * Uint64.t) list =
  let open PS.Node in
  let rec loop acc curr_node =
    let scope_id = scopeId_get curr_node in
    if Util.uint64_equal scope_id Uint64.zero then
      acc
    else
      let parent = Hashtbl.find_exn nodes_table scope_id in
      let node_name = get_unqualified_name ~parent ~child:curr_node in
      let node_id = id_get curr_node in
      loop ((node_name, node_id) :: acc) parent
  in
  loop [] node


(* Get the fully-qualified name for [node]. *)
let get_fully_qualified_name nodes_table node : string =
  get_fully_qualified_name_components nodes_table node |>
  List.map ~f:fst |>
  String.concat ~sep:"."


(* Get a qualified module name for [node] which is suitable for use at the given
 * [scope_stack] position. *)
let get_scope_relative_name nodes_table (scope_stack : Uint64.t list) node
  : string =
  let rec pop_components components scope =
    match components, scope with
    | ( (component_name, component_scope_id) ::
          other_components, scope_id :: scope_ids) ->
        if Util.uint64_equal component_scope_id scope_id then
          pop_components other_components scope_ids
        else
          components
    | _ ->
        components
  in
  let fq_name = get_fully_qualified_name_components nodes_table node in
  let rel_name = pop_components fq_name (List.rev scope_stack) in
  String.concat ~sep:"." (List.map rel_name ~f:fst)


let make_unique_typename ~(mode : Mode.t) ~(scope_mode : Mode.t) ~nodes_table node =
  let uq_name = get_unqualified_name
    ~parent:(Hashtbl.find_exn nodes_table (PS.Node.scopeId_get node)) ~child:node
  in
  let t_str =
    match (mode, scope_mode) with
    | (Mode.Reader, Mode.Reader)
    | (Mode.Builder, Mode.Builder) ->
        "t"
    | (Mode.Reader, Mode.Builder) ->
        "reader_t"
    | (Mode.Builder, Mode.Reader) ->
        "builder_t"
  in
  sprintf "%s_%s_%s" t_str uq_name (Uint64.to_string (PS.Node.id_get node))


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
 * based on the 64-bit unique ID for Foo. *)
let make_disambiguated_type_name ~(mode : Mode.t) ~(scope_mode : Mode.t)
    ~nodes_table ~scope ~tp node =
  let node_id = PS.Node.id_get node in
  if List.mem scope node_id then
    (* The node of interest is a parent node of the node being generated.
       this is the case where an unambiguous type is emitted. *)
    make_unique_typename ~mode ~scope_mode ~nodes_table node
  else
    let module_name = get_scope_relative_name nodes_table scope node in
    let t_str =
      match PS.Type.get tp with
      | PS.Type.Enum _ ->
          (* Enum types are identical across reader and builder, no need
             to distinguish between them *)
          ".t"
      | _ ->
          begin match (mode, scope_mode) with
          | (Mode.Reader, Mode.Reader)
          | (Mode.Builder, Mode.Builder) ->
              ".t"
          | (Mode.Reader, Mode.Builder) ->
              ".reader_t"
          | (Mode.Builder, Mode.Reader) ->
              ".builder_t"
          end
    in
    module_name ^ t_str


(* Construct an ocaml name for the given schema-defined type.
   [mode] indicates whether the generated type name represents a Reader or a
   Builder type.  [scope_mode] indicates whether the generated type name is
   to be referenced within the scope of a Reader or a Builder. *)
let rec type_name ~(mode : Mode.t) ~(scope_mode : Mode.t)
    nodes_table scope tp : string =
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
      let list_type = List.elementType_get list_descr in
      sprintf "(%s, %s, %s) Runtime.Array.t"
        (if mode = Mode.Reader then "ro" else "rw")
        (type_name ~mode ~scope_mode nodes_table scope list_type)
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
      let enum_id = Enum.typeId_get enum_descr in
      let enum_node = Hashtbl.find_exn nodes_table enum_id in
      make_disambiguated_type_name ~mode ~scope_mode ~nodes_table
        ~scope ~tp enum_node
  | Struct struct_descr ->
      let struct_id = Struct.typeId_get struct_descr in
      let struct_node = Hashtbl.find_exn nodes_table struct_id in
      make_disambiguated_type_name ~mode ~scope_mode ~nodes_table
        ~scope ~tp struct_node
  | Interface iface_descr ->
      let iface_id = Interface.typeId_get iface_descr in
      let iface_node = Hashtbl.find_exn nodes_table iface_id in
      make_disambiguated_type_name ~mode ~scope_mode ~nodes_table
        ~scope ~tp iface_node
  | AnyPointer ->
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


let generate_union_type ~(mode : Mode.t) nodes_table scope fields =
  let open PS.Field in
  let cases = List.fold_left fields ~init:[] ~f:(fun acc field ->
    let field_name = String.capitalize (name_get field) in
    match get field with
    | Slot slot ->
        let field_type = Slot.type_get slot in
        begin match PS.Type.get field_type with
        | PS.Type.Void ->
            ("  | " ^ field_name) :: acc
        | _ ->
            ("  | " ^ field_name ^ " of " ^
               (type_name ~mode ~scope_mode:mode nodes_table scope field_type))
            :: acc
        end
    | Group group ->
        let group_type_name =
          let group_id = Group.typeId_get group in
          let group_node = Hashtbl.find_exn nodes_table group_id in
          let group_module_name =
            get_scope_relative_name nodes_table scope group_node
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


(* Generate the signature for an enum type. *)
let generate_enum_sig ~nodes_table ~scope ~nested_modules
    ~mode ~node enum_def =
  let is_builder = mode = Mode.Builder in
  let header =
    if is_builder then
      let reader_type_string =
        "Reader." ^ (get_fully_qualified_name nodes_table node) ^ ".t"
      in
      [ "type t = " ^ reader_type_string ^ " =" ]
    else
      [ "type t =" ]
  in
  let variants =
    let enumerants = PS.Node.Enum.enumerants_get enum_def in
    let undefined_name = mangle_enum_undefined enumerants in
    let footer = [
      sprintf "  | %s of int" (String.capitalize undefined_name)
    ] in
    RT.Array.fold_right enumerants ~init:footer ~f:(fun enumerant acc ->
      let name = String.capitalize (PS.Enumerant.name_get enumerant) in
      let match_case = "  | " ^ name in
      match_case :: acc)
  in
  nested_modules @ header @ variants


let generate_constant ~nodes_table ~scope const_def =
  let open PS.Value in
  let const_val = PS.Node.Const.value_get const_def in
  match get const_val with
  | Void ->
      "()"
  | Bool a ->
      if a then "true" else "false"
  | Int8 a
  | Int16 a
  | Uint8 a
  | Uint16 a ->
      Int.to_string a
  | Int32 a ->
      (Int32.to_string a) ^ "l"
  | Int64 a ->
      (Int64.to_string a) ^ "L"
  | Uint32 a ->
      sprintf "(Uint32.of_string %s)" (Uint32.to_string a)
  | Uint64 a ->
      sprintf "(Uint64.of_string %s)" (Uint64.to_string a)
  | Float32 a ->
      sprintf "(Int32.float_of_bits %sl)"
        (Int32.to_string (Int32.bits_of_float a))
  | Float64 a ->
      sprintf "(Int64.float_of_bits %sL)"
        (Int64.to_string (Int64.bits_of_float a))
  | Text a
  | Data a ->
      "\"" ^ (String.escaped a) ^ "\""
  | List _ ->
      failwith "List constants are not yet implemented."
  | Enum enum_val ->
      let const_type = PS.Node.Const.type_get const_def in
      let enum_node =
        match PS.Type.get const_type with
        | PS.Type.Enum enum_def ->
            let enum_id = PS.Type.Enum.typeId_get enum_def in
            Hashtbl.find_exn nodes_table enum_id
        | _ ->
            failwith "Decoded non-enum node where enum node was expected."
      in
      let enumerants =
        match PS.Node.get enum_node with
        | PS.Node.Enum enum_group -> PS.Node.Enum.enumerants_get enum_group
        | _ -> failwith "Decoded non-enum node where enum node was expected."
      in
      let undefined_name = mangle_enum_undefined enumerants in
      let scope_relative_name =
        get_scope_relative_name nodes_table scope enum_node in
      if enum_val >= RT.Array.length enumerants then
        sprintf "%s.%s %u" scope_relative_name
          (String.capitalize undefined_name) enum_val
      else
        let enumerant = RT.Array.get enumerants enum_val in
        sprintf "%s.%s"
          scope_relative_name
          (String.capitalize (PS.Enumerant.name_get enumerant))
  | Struct _ ->
      failwith "Struct constants are not yet implemented."
  | Interface ->
      failwith "Interface constants are not yet implemented."
  | AnyPointer _ ->
      failwith "AnyPointer constants are not yet implemented."
  | Undefined x ->
      failwith (sprintf "Unknown Value union discriminant %u." x)

