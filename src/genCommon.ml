
open Core.Std

module M  = Message.Make(StrStorage)
module PS = PluginSchema.Make(M)
module R  = Runtime


let children_of
    (nodes_table : (Uint64.t, PS.Node.t) Hashtbl.t)
    (parent : PS.Node.t)
: PS.Node.t list =
  let parent_id = PS.Node.id_get parent in
  Hashtbl.fold nodes_table ~init:[] ~f:(fun ~key:id ~data:node acc ->
    if Util.uint64_equal parent_id (PS.Node.scopeId_get node) then
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
  let child_id = PS.Node.id_get child in
  let nested_nodes = PS.Node.nestedNodes_get parent in
  let rec loop_nested_nodes i =
    if i = R.Array.length nested_nodes then
      None
    else
      let nested_node = R.Array.get nested_nodes i in
      if Util.uint64_equal child_id (PS.Node.NestedNode.id_get nested_node) then
        Some (PS.Node.NestedNode.name_get nested_node)
      else
        loop_nested_nodes (i + 1)
  in
  match loop_nested_nodes 0 with
  | Some s ->
      String.capitalize s
  | None ->
      let error_msg = Printf.sprintf
        "Unable to find unqualified name of child node %s (%s) within parent node %s (%s)."
        (Uint64.to_string child_id)
        (PS.Node.displayName_get child)
        (Uint64.to_string (PS.Node.id_get parent))
        (PS.Node.displayName_get parent)
      in
      begin match PS.Node.unnamed_union_get parent with
      | PS.Node.File
      | PS.Node.Enum _
      | PS.Node.Interface _
      | PS.Node.Const _
      | PS.Node.Annotation _ ->
          failwith error_msg
      | PS.Node.Struct node_struct ->
          let fields = PS.Node.Struct.fields_get node_struct in
          let rec loop_fields i =
            if i = R.Array.length fields then
              failwith error_msg
            else
              let field = R.Array.get fields i in
              match PS.Field.unnamed_union_get field with
              | PS.Field.Slot _ ->
                  loop_fields (i + 1)
              | PS.Field.Group group ->
                  if Util.uint64_equal child_id (PS.Field.Group.typeId_get group) then
                    String.capitalize (PS.Field.name_get field)
                  else
                    loop_fields (i + 1)
              | PS.Field.Undefined_ x ->
                  failwith (Printf.sprintf "Unknown Field union discriminant %d" x)
          in
          loop_fields 0
      | PS.Node.Undefined_ x ->
          failwith (Printf.sprintf "Unknown Node union discriminant %d" x)
      end


(* Get a representation of the fully-qualified module name for [node].
 * The resulting list associates each component of the name with scope which it
 * defines.  The head of the list is at the outermost scope. *)
let get_fully_qualified_name nodes_table node : (string * Uint64.t) list =
  let rec loop acc curr_node =
    let scope_id = PS.Node.scopeId_get curr_node in
    if Util.uint64_equal scope_id Uint64.zero then
      acc
    else
      let parent = Hashtbl.find_exn nodes_table scope_id in
      (get_unqualified_name ~parent ~child:curr_node, PS.Node.id_get node) :: acc
  in
  loop [] node


(* Get a the qualified module name for [node] which is suitable for use at the given
 * [scope_stack] position. *)
let get_scope_relative_name nodes_table (scope_stack : Uint64.t list) node : string =
  let rec pop_components components scope =
    match components, scope with
    | ( (component_name, component_scope_id) :: other_components, scope_id :: scope_ids) ->
        if Util.uint64_equal component_scope_id scope_id then
          pop_components other_components scope_ids
        else
          components
    | _ ->
        components
  in
  let fq_name = get_fully_qualified_name nodes_table node in
  let rel_name = pop_components fq_name (List.rev scope_stack) in
  String.concat ~sep:"." (List.map rel_name ~f:fst)


(* Construct an ocaml name for the given schema-defined type. *)
let rec type_name nodes_table scope tp : string =
  match PS.Type.unnamed_union_get tp with
  | PS.Type.Void    -> "unit"
  | PS.Type.Bool    -> "bool"
  | PS.Type.Int8    -> "int"
  | PS.Type.Int16   -> "int"
  | PS.Type.Int32   -> "int32"
  | PS.Type.Int64   -> "int64"
  | PS.Type.Uint8   -> "int"
  | PS.Type.Uint16  -> "int"
  | PS.Type.Uint32  -> "Uint32.t"
  | PS.Type.Uint64  -> "Uint64.t"
  | PS.Type.Float32 -> "float"
  | PS.Type.Float64 -> "float"
  | PS.Type.Text    -> "string"
  | PS.Type.Data    -> "string"
  | PS.Type.List list_descr ->
      let list_type = PS.Type.List.elementType_get list_descr in
      Printf.sprintf "(%s, array_t) Runtime.Array.t" (type_name nodes_table scope list_type)
  | PS.Type.Enum enum_descr ->
      let enum_id = PS.Type.Enum.typeId_get enum_descr in
      let enum_node = Hashtbl.find_exn nodes_table enum_id in
      let enum_module_name = get_scope_relative_name nodes_table scope enum_node in
      enum_module_name ^ ".t"
  | PS.Type.Struct struct_descr ->
      let struct_id = PS.Type.Struct.typeId_get struct_descr in
      let struct_node = Hashtbl.find_exn nodes_table struct_id in
      let struct_module_name = get_scope_relative_name nodes_table scope struct_node in
      struct_module_name ^ ".t"
  | PS.Type.Interface iface_descr ->
      let iface_id = PS.Type.Interface.typeId_get iface_descr in
      let iface_node = Hashtbl.find_exn nodes_table iface_id in
      let iface_module_name = get_scope_relative_name nodes_table scope iface_node in
      iface_module_name ^ ".t"
  | PS.Type.Object ->
      "AnyPointer.t"
  | PS.Type.Undefined_ x ->
      failwith (Printf.sprintf "Unknown Type union discriminant %d" x)


(* Generate a variant type declaration for a capnp union type. *)
let generate_union_type nodes_table scope struct_def fields =
  let indent = String.make (2 * (List.length scope + 1)) ' ' in
  let cases = List.fold_left fields ~init:[] ~f:(fun acc field ->
    let field_name = String.capitalize (PS.Field.name_get field) in
    match PS.Field.unnamed_union_get field with
    | PS.Field.Slot slot ->
        let field_type = PS.Field.Slot.type_get slot in
        begin match PS.Type.unnamed_union_get field_type with
        | PS.Type.Void ->
            (Printf.sprintf "%s  | %s" indent field_name) :: acc
        | _ ->
            (Printf.sprintf "%s  | %s of %s" indent field_name
              (type_name nodes_table scope field_type)) :: acc
        end
    | PS.Field.Group group ->
        let group_type_name =
          let group_id = PS.Field.Group.typeId_get group in
          let group_node = Hashtbl.find_exn nodes_table group_id in
          let group_module_name = get_scope_relative_name nodes_table scope group_node in
          group_module_name ^ ".t"
        in
        (Printf.sprintf "%s  | %s of %s" indent field_name group_type_name) :: acc
    | PS.Field.Undefined_ x ->
        failwith (Printf.sprintf "Unknown Field union discriminant %d" x))
  in
  let header = [
    Printf.sprintf "%stype unnamed_union_t =" indent;
  ] in
  let footer = [
    Printf.sprintf "%s  | Undefined_ of int\n" indent
  ] in
  String.concat ~sep:"\n" (header @ cases @ footer)


(* Generate the signature for an enum type. *)
let generate_enum_sig ~nodes_table ~scope ~nested_modules enum_def =
  let indent = String.make (2 * (List.length scope + 1)) ' ' in
  let header = Printf.sprintf "%stype t =\n" indent in
  let variants =
    let enumerants = PS.Node.Enum.enumerants_get enum_def in
    let buf = Buffer.create 512 in
    for i = 0 to R.Array.length enumerants - 1 do
      let enumerant = R.Array.get enumerants i in
      let match_case =
        Printf.sprintf "%s  | %s\n"
          indent
          (String.capitalize (PS.Enumerant.name_get enumerant))
      in
      Buffer.add_string buf match_case
    done;
    let footer = Printf.sprintf "%s  | Undefined_ of int\n" indent in
    let () = Buffer.add_string buf footer in
    Buffer.contents buf
  in
  nested_modules ^ header ^ variants


