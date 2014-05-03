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

module M  = Message.Make(StrStorage)
module PS = PluginSchema.Make(M)
module RT = Runtime

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
  let enumerant_names = RT.Array.map_list enumerants ~f:PS.Enumerant.R.name_get in
  mangle_undefined enumerant_names

let mangle_field_undefined (fields : 'a list) =
  let field_names = List.rev_map fields ~f:PS.Field.R.name_get in
  mangle_undefined field_names


let children_of
    (nodes_table : (Uint64.t, PS.Node.reader_t) Hashtbl.t)
    (parent : PS.Node.reader_t)
: PS.Node.reader_t list =
  let open PS.Node in
  let parent_id = R.id_get parent in
  Hashtbl.fold nodes_table ~init:[] ~f:(fun ~key:id ~data:node acc ->
    if Util.uint64_equal parent_id (R.scopeId_get node) then
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
    ~(parent : PS.Node.reader_t)
    ~(child  : PS.Node.reader_t)
: string =
  let open PS.Node in
  let child_id = R.id_get child in
  let nested_nodes = R.nestedNodes_get parent in
  let matching_nested_node_name =
    RT.Array.find_map nested_nodes ~f:(fun nested_node ->
      if Util.uint64_equal child_id (NestedNode.R.id_get nested_node) then
        Some (NestedNode.R.name_get nested_node)
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
        (R.displayName_get child)
        (Uint64.to_string (R.id_get parent))
        (R.displayName_get parent)
      in
      begin match R.get parent with
      | R.File
      | R.Enum _
      | R.Interface _
      | R.Const _
      | R.Annotation _ ->
          failwith error_msg
      | R.Struct node_struct ->
          let fields = Struct.R.fields_get node_struct in
          let matching_field_name =
            RT.Array.find_map fields ~f:(fun field ->
              match PS.Field.R.get field with
              | PS.Field.R.Slot _ ->
                  None
              | PS.Field.R.Group group ->
                  if Util.uint64_equal child_id
                      (PS.Field.Group.R.typeId_get group) then
                    Some (String.capitalize (PS.Field.R.name_get field))
                  else
                    None
              | PS.Field.R.Undefined x ->
                  failwith (sprintf "Unknown Field union discriminant %d" x))
          in
          begin match matching_field_name with
          | Some name -> name
          | None      -> failwith error_msg
          end
      | PS.Node.R.Undefined x ->
          failwith (sprintf "Unknown Node union discriminant %d" x)
      end


(* Get a representation of the fully-qualified module name for [node].
 * The resulting list associates each component of the name with the scope
 * which it defines.  The head of the list is at the outermost scope. *)
let get_fully_qualified_name_components nodes_table node
  : (string * Uint64.t) list =
  let open PS.Node in
  let rec loop acc curr_node =
    let scope_id = R.scopeId_get curr_node in
    if Util.uint64_equal scope_id Uint64.zero then
      acc
    else
      let parent = Hashtbl.find_exn nodes_table scope_id in
      let node_name = get_unqualified_name ~parent ~child:curr_node in
      let node_id = R.id_get curr_node in
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


let make_unique_typename ~(mode : Mode.t) ~nodes_table node =
  let uq_name = get_unqualified_name
    ~parent:(Hashtbl.find_exn nodes_table (PS.Node.R.scopeId_get node)) ~child:node
  in
  let t_str =
    match mode with
    | Mode.Reader  -> "reader_t"
    | Mode.Builder -> "builder_t"
  in
  sprintf "%s_%s_%s" t_str uq_name (Uint64.to_string (PS.Node.R.id_get node))


(* When modules refer to types defined in other modules, readability dictates that we use
 * OtherModule.reader_t/OtherModule.builder_t as the preferred type name.  However,
 * consider the case of nested modules:
 *
 * module Foo : sig
 *   type reader_t
 *   type builder_t
 *   type reader_t_Foo_UID = reader_t
 *   type builder_t_Foo_UID = builder_t
 *
 *   module Bar : sig
 *     type reader_t
 *     type builder_t
 *     type reader_t_Bar_UID = reader_t
 *     type builder_t_Bar_UID = builder_t
 *
 *     module R : sig
 *       val foo_get : t -> reader_t_Foo_UID
 *     end
 *     module B : sig
 *       val foo_get : t -> builder_t_Foo_UID
 *       val foo_set_reader : t -> reader_t_Foo_UID -> builder_t_Foo_UID
 *       val foo_set_builder : t -> builder_t_Foo_UID -> builder_t_Foo_UID
 *   end
 *   ...
 * end
 *
 * In this case, module Foo does not have a complete declaration at the time foo_get is
 * declared, so we can't refer to the type as "Foo.reader_t".  So for this case instead
 * of using Foo.reader_t we emit an unambiguous type identifier based on the 64-bit
 * unique ID for Foo. *)
let make_disambiguated_type_name ~(mode : Mode.t) ~(scope_mode : Mode.t)
    ~nodes_table ~scope ~tp node =
  let node_id = PS.Node.R.id_get node in
  if List.mem scope node_id then
    (* The node of interest is a parent node of the node being generated.
       this is the case where an unambiguous type is emitted. *)
    make_unique_typename ~mode ~nodes_table node
  else
    let module_name = get_scope_relative_name nodes_table scope node in
    let t_str =
      match PS.Type.R.get tp with
      | PS.Type.R.Enum _ ->
          (* Enum types are identical across reader and builder, no need
             to distinguish between them *)
          ".t"
      | _ ->
          begin match mode with
          | Mode.Reader  -> ".reader_t"
          | Mode.Builder -> ".builder_t"
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
  match R.get tp with
  | R.Void    -> "unit"
  | R.Bool    -> "bool"
  | R.Int8    -> "int"
  | R.Int16   -> "int"
  | R.Int32   -> "int32"
  | R.Int64   -> "int64"
  | R.Uint8   -> "int"
  | R.Uint16  -> "int"
  | R.Uint32  -> "Uint32.t"
  | R.Uint64  -> "Uint64.t"
  | R.Float32 -> "float"
  | R.Float64 -> "float"
  | R.Text    -> "string"
  | R.Data    -> "string"
  | R.List list_descr ->
      let list_type = List.R.elementType_get list_descr in
      sprintf "(%s, %s, %s) Runtime.Array.t"
        (if mode = Mode.Reader then "ro" else "rw")
        (type_name ~mode ~scope_mode nodes_table scope list_type)
        (if mode = Mode.Reader then "reader_array_t" else "builder_array_t")
  | R.Enum enum_descr ->
      let enum_id = Enum.R.typeId_get enum_descr in
      let enum_node = Hashtbl.find_exn nodes_table enum_id in
      make_disambiguated_type_name ~mode ~scope_mode ~nodes_table
        ~scope ~tp enum_node
  | R.Struct struct_descr ->
      let struct_id = Struct.R.typeId_get struct_descr in
      let struct_node = Hashtbl.find_exn nodes_table struct_id in
      make_disambiguated_type_name ~mode ~scope_mode ~nodes_table
        ~scope ~tp struct_node
  | R.Interface iface_descr ->
      let iface_id = Interface.R.typeId_get iface_descr in
      let iface_node = Hashtbl.find_exn nodes_table iface_id in
      make_disambiguated_type_name ~mode ~scope_mode ~nodes_table
        ~scope ~tp iface_node
  | R.AnyPointer ->
      begin match mode with
      | Mode.Reader  -> "AnyPointer.reader_t"
      | Mode.Builder -> "AnyPointer.builder_t"
      end
  | R.Undefined x ->
      failwith (sprintf "Unknown Type union discriminant %d" x)


let generate_union_type ~(mode : Mode.t) nodes_table scope fields =
  let open PS.Field in
  let cases = List.fold_left fields ~init:[] ~f:(fun acc field ->
    let field_name = String.capitalize (R.name_get field) in
    match R.get field with
    | R.Slot slot ->
        let field_type = Slot.R.type_get slot in
        begin match PS.Type.R.get field_type with
        | PS.Type.R.Void ->
            ("  | " ^ field_name) :: acc
        | _ ->
            ("  | " ^ field_name ^ " of " ^
               (type_name ~mode ~scope_mode:mode nodes_table scope field_type))
            :: acc
        end
    | R.Group group ->
        let group_type_name =
          let group_id = Group.R.typeId_get group in
          let group_node = Hashtbl.find_exn nodes_table group_id in
          let group_module_name =
            get_scope_relative_name nodes_table scope group_node
          in
          let t_str = 
            match mode with
            | Mode.Reader -> ".reader_t"
            | Mode.Builder -> ".builder_t"
          in
          group_module_name ^ t_str
        in
        ("  | " ^ field_name ^ " of " ^ group_type_name) :: acc
    | R.Undefined x ->
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
    let enumerants = PS.Node.Enum.R.enumerants_get enum_def in
    let undefined_name = mangle_enum_undefined enumerants in
    let footer = [
      sprintf "  | %s of int" (String.capitalize undefined_name)
    ] in
    RT.Array.fold_right enumerants ~init:footer ~f:(fun enumerant acc ->
      let name = String.capitalize (PS.Enumerant.R.name_get enumerant) in
      let match_case = "  | " ^ name in
      match_case :: acc)
  in
  nested_modules @ header @ variants


let generate_constant ~nodes_table ~scope const_def =
  let open PS.Value in
  let const_val = PS.Node.Const.R.value_get const_def in
  match R.get const_val with
  | R.Void ->
      "()"
  | R.Bool a ->
      if a then "true" else "false"
  | R.Int8 a
  | R.Int16 a
  | R.Uint8 a
  | R.Uint16 a ->
      Int.to_string a
  | R.Int32 a ->
      (Int32.to_string a) ^ "l"
  | R.Int64 a ->
      (Int64.to_string a) ^ "L"
  | R.Uint32 a ->
      sprintf "(Uint32.of_string %s)" (Uint32.to_string a)
  | R.Uint64 a ->
      sprintf "(Uint64.of_string %s)" (Uint64.to_string a)
  | R.Float32 a ->
      sprintf "(Int32.float_of_bits %sl)"
        (Int32.to_string (Int32.bits_of_float a))
  | R.Float64 a ->
      sprintf "(Int64.float_of_bits %sL)"
        (Int64.to_string (Int64.bits_of_float a))
  | R.Text a
  | R.Data a ->
      "\"" ^ (String.escaped a) ^ "\""
  | R.List _ ->
      failwith "List constants are not yet implemented."
  | R.Enum enum_val ->
      let const_type = PS.Node.Const.R.type_get const_def in
      let enum_node =
        match PS.Type.R.get const_type with
        | PS.Type.R.Enum enum_def ->
            let enum_id = PS.Type.Enum.R.typeId_get enum_def in
            Hashtbl.find_exn nodes_table enum_id
        | _ ->
            failwith "Decoded non-enum node where enum node was expected."
      in
      let enumerants =
        match PS.Node.R.get enum_node with
        | PS.Node.R.Enum enum_group -> PS.Node.Enum.R.enumerants_get enum_group
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
          (String.capitalize (PS.Enumerant.R.name_get enumerant))
  | R.Struct _ ->
      failwith "Struct constants are not yet implemented."
  | R.Interface ->
      failwith "Interface constants are not yet implemented."
  | R.AnyPointer _ ->
      failwith "AnyPointer constants are not yet implemented."
  | R.Undefined x ->
      failwith (sprintf "Unknown Value union discriminant %u." x)

