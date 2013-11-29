
open Core.Std

module PS = PluginSchema.Make(StrStorage)
module CArray = PS.CapnpArray


let children_of
    (nodes_table : (Uint64.t, Message.ro PS.Node.t) Hashtbl.t)
    (parent : Message.ro PS.Node.t)
: Message.ro PS.Node.t list =
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
    ~(parent : Message.ro PS.Node.t)
    ~(child  : Message.ro PS.Node.t)
: string =
  let child_id = PS.Node.id_get child in
  let nested_nodes = PS.Node.nestedNodes_get parent in
  let rec loop_nested_nodes i =
    if i = CArray.length nested_nodes then
      None
    else
      let nested_node = CArray.get nested_nodes i in
      if Util.uint64_equal child_id (PS.Node.NestedNode.id_get nested_node) then
        Some (PS.Node.NestedNode.name_get nested_node)
      else
        loop_nested_nodes (i + 1)
  in
  match loop_nested_nodes 0 with
  | Some s ->
      s
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
            if i = CArray.length fields then
              failwith error_msg
            else
             let field = CArray.get fields i in
             match PS.Field.unnamed_union_get field with
             | PS.Field.Slot _ ->
                 loop_fields (i + 1)
             | PS.Field.Group group ->
                 if Util.uint64_equal child_id (PS.Field.Group.typeId_get group) then
                   PS.Field.name_get field
                  else
                    loop_fields (i + 1)
          in
          loop_fields 0
      end


(* Get a representation of the fully-qualified module name for [node].
 * The resulting list associates each component of the name with the parent scope
 * in which it is defined.  The head of the list is at the outermost scope. *)
let get_fully_qualified_name nodes_table node : (string * Uint64.t) list =
  let rec loop acc curr_node =
    let scope_id = PS.Node.scopeId_get curr_node in
    if Util.uint64_equal scope_id Uint64.zero then
      acc
    else
      let parent = Hashtbl.find_exn nodes_table scope_id in
      (get_unqualified_name ~parent ~child:curr_node, scope_id) :: acc
  in
  loop [] node


(* Get a the qualified module name for [node] which is suitable for use at the given
 * [scope_stack} position. *)
let get_scope_relative_name nodes_table (scope_stack : Uint64.t list) node =
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


let no_discriminant = 0xffff


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
      (type_name nodes_table scope list_type) ^ " list"
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
      "Object.t"


let generate_union_type nodes_table scope struct_def fields =
  let indent = String.make (2 * (List.length scope + 1)) ' ' in
  let cases = List.fold_left fields ~init:[] ~f:(fun acc field ->
    let field_name = PS.Field.name_get field in
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
        (Printf.sprintf "%s  | %s of %s" indent field_name group_type_name) :: acc)
  in
  let header = [
    Printf.sprintf "%stype unnamed_union_t =" indent;
  ] in
  let footer = [
    Printf.sprintf "%s  | Undefined_ of int\n" indent
  ] in
  String.concat ~sep:"\n" (header @ cases @ footer)


let generate_union_accessors nodes_table scope struct_def fields =
  let indent = String.make (2 * (List.length scope + 1)) ' ' in
  let cases = List.fold_left fields ~init:[] ~f:(fun acc field ->
    let field_name  = PS.Field.name_get field in
    let field_value = PS.Field.discriminantValue_get field in
    (Printf.sprintf "%s  | %u -> %s"
      indent
      field_value
      field_name) :: acc)
  in
  let header = [
    Printf.sprintf "%slet unnamed_union_get x =" indent;
    Printf.sprintf "%s  match get_struct_field_uint16 x %u with"
      indent ((Uint32.to_int (PS.Node.Struct.discriminantOffset_get struct_def)) * 2);
  ] in
  let footer = [
    Printf.sprintf "%s  | v -> Undefined_ v\n" indent
  ] in
  (generate_union_type nodes_table scope struct_def fields) ^ "\n\n" ^
  String.concat ~sep:"\n" (header @ cases @ footer)


let generate_list_accessor ~list_type ~indent ~field_name ~field_ofs =
  match PS.Type.unnamed_union_get list_type with
  | PS.Type.Void ->
      Printf.sprintf "%slet %s_get x = failwith \"not implemented\"\n"
        indent
        field_name
  | PS.Type.Bool ->
      Printf.sprintf "%slet %s_get x = get_struct_field_bit_list x %u\n"
        indent
        field_name
        field_ofs
  | PS.Type.Int8 ->
      Printf.sprintf "%slet %s_get x = get_struct_field_int8_list x %u\n"
        indent
        field_name
        field_ofs
  | PS.Type.Int16 ->
      Printf.sprintf "%slet %s_get x = get_struct_field_int16_list x %u\n"
        indent
        field_name
        field_ofs
  | PS.Type.Int32 ->
      Printf.sprintf "%slet %s_get x = get_struct_field_int32_list x %u\n"
        indent
        field_name
        field_ofs
  | PS.Type.Int64 ->
      Printf.sprintf "%slet %s_get x = get_struct_field_int64_list x %u\n"
        indent
        field_name
        field_ofs
  | PS.Type.Uint8 ->
      Printf.sprintf "%slet %s_get x = get_struct_field_uint8_list x %u\n"
        indent
        field_name
        field_ofs
  | PS.Type.Uint16 ->
      Printf.sprintf "%slet %s_get x = get_struct_field_uint16_list x %u\n"
        indent
        field_name
        field_ofs
  | PS.Type.Uint32 ->
      Printf.sprintf "%slet %s_get x = get_struct_field_uint32_list x %u\n"
        indent
        field_name
        field_ofs
  | PS.Type.Uint64 ->
      Printf.sprintf "%slet %s_get x = get_struct_field_uint64_list x %u\n"
        indent
        field_name
        field_ofs
  | PS.Type.Float32
  | PS.Type.Float64 ->
      Printf.sprintf "%slet %s_get x = failwith \"not implemented\"\n"
        indent
        field_name
  | PS.Type.Text ->
      Printf.sprintf "%slet %s_get x = get_struct_field_text_list x %u\n"
        indent
        field_name
        field_ofs
  | PS.Type.Data ->
      Printf.sprintf "%slet %s_get x = get_struct_field_blob_list x %u\n"
        indent
        field_name
        field_ofs
  | PS.Type.List _ ->
      Printf.sprintf "%slet %s_get x = get_struct_field_list_list x %u\n"
        indent
        field_name
        field_ofs
  | PS.Type.Enum _ ->
      Printf.sprintf "%slet %s_get x = failwith \"not implemented\"\n"
        indent
        field_name
  | PS.Type.Struct _ ->
      Printf.sprintf "%slet %s_get x = get_struct_field_struct_list x %u\n"
        indent
        field_name
        field_ofs
  | PS.Type.Interface _ ->
      Printf.sprintf "%slet %s_get x = failwith \"not implemented\"\n"
        indent
        field_name
  | PS.Type.Object ->
      Printf.sprintf "%slet %s_get x = failwith \"not implemented\"\n"
        indent
        field_name


let generate_non_union_accessors nodes_table scope struct_def fields =
  let indent = String.make (2 * (List.length scope + 1)) ' ' in
  let accessors = List.fold_left fields ~init:[] ~f:(fun acc field ->
    let field_accessors : string =
      let field_name = PS.Field.name_get field in
      match PS.Field.unnamed_union_get field with
      | PS.Field.Group group ->
          Printf.sprintf "%slet %s_get x = x\n"
            indent
            field_name
      | PS.Field.Slot slot ->
          let field_ofs  = Uint32.to_int (PS.Field.Slot.offset_get slot) in
          let tp = PS.Field.Slot.type_get slot in
          begin match PS.Type.unnamed_union_get tp with
          | PS.Type.Void ->
              Printf.sprintf "%slet %s_get x = ()\n"
                indent
                field_name
          | PS.Type.Bool ->
              let byte_ofs    = field_ofs / 8 in
              let bit_in_byte = field_ofs mod 8 in
              Printf.sprintf "%slet %s_get x = get_struct_field_bit x %u %u\n"
                indent
                field_name
                byte_ofs
                bit_in_byte
          | PS.Type.Int8 ->
              Printf.sprintf "%slet %s_get x = get_struct_field_int8 x %u\n"
                indent
                field_name
                field_ofs
          | PS.Type.Int16 ->
              Printf.sprintf "%slet %s_get x = get_struct_field_int16 x %u\n"
                indent
                field_name
                (2 * field_ofs)
          | PS.Type.Int32 ->
              String.concat ~sep:"\n" [
                Printf.sprintf "%slet %s_get x = get_struct_field_int32 x %u\n"
                  indent
                  field_name
                  (4 * field_ofs);
                Printf.sprintf "%slet %s_get_int_exn x = Int32.to_int (%s_get x)\n"
                  indent
                  field_name
                  field_name
              ]
          | PS.Type.Int64 ->
              String.concat ~sep:"\n" [
                Printf.sprintf "%slet %s_get x = get_struct_field_int64 x %u\n"
                  indent
                  field_name
                  (8 * field_ofs);
                Printf.sprintf "%slet %s_get_int_exn x = Int64.to_int (%s_get x)\n"
                  indent
                  field_name
                  field_name
              ]
          | PS.Type.Uint8 ->
              Printf.sprintf "%slet %s_get x = get_struct_field_uint8 x %u\n"
                indent
                field_name
                field_ofs
          | PS.Type.Uint16 ->
              Printf.sprintf "%slet %s_get x = get_struct_field_uint16 x %u\n"
                indent
                field_name
                (2 * field_ofs)
          | PS.Type.Uint32 ->
              String.concat ~sep:"\n" [
                Printf.sprintf "%slet %s_get x = get_struct_field_uint32 x %u\n"
                  indent
                  field_name
                  (4 * field_ofs);
                Printf.sprintf "%slet %s_get_int_exn x = Uint32.to_int (%s_get x)\n"
                  indent
                  field_name
                  field_name
              ]
          | PS.Type.Uint64 ->
              String.concat ~sep:"\n" [
                Printf.sprintf "%slet %s_get x = get_struct_field_uint64 x %u\n"
                  indent
                  field_name
                  (8 * field_ofs);
                Printf.sprintf "%slet %s_get_int_exn x = Uint64.to_int (%s_get x)\n"
                  indent
                  field_name
                  field_name
              ]
          | PS.Type.Float32 ->
              Printf.sprintf "%slet %s_get x = failwith \"not implemented\"\n"
                indent
                field_name
          | PS.Type.Float64 ->
              Printf.sprintf "%slet %s_get x = failwith \"not implemented\"\n"
                indent
                field_name
          | PS.Type.Text ->
              Printf.sprintf "%slet %s_get x = get_struct_field_text x %u\n"
                indent
                field_name
                field_ofs
          | PS.Type.Data ->
              Printf.sprintf "%slet %s_get x = get_struct_field_blob x %u\n"
                indent
                field_name
                field_ofs
          | PS.Type.List list ->
              let list_type = PS.Type.List.elementType_get list in
              generate_list_accessor ~list_type ~indent ~field_name ~field_ofs
          | PS.Type.Enum enum ->
              Printf.sprintf "%slet %s_get x = failwith \"not implemented\"\n"
                indent
                field_name
          | PS.Type.Struct struct_def ->
              Printf.sprintf "%slet %s_get x = get_struct_field_struct x %u\n"
                indent
                field_name
                field_ofs
          | PS.Type.Interface iface ->
              Printf.sprintf "%slet %s_get x = failwith \"not implemented\"\n"
                indent
                field_name
          | PS.Type.Object ->
              Printf.sprintf "%slet %s_get x = failwith \"not implemented\"\n"
                indent
                field_name
          end
    in
    (field_accessors :: acc))
  in
  String.concat ~sep:"\n" accessors


(* Generate the OCaml module corresponding to a struct definition.  [scope] is a
 * stack of scope IDs corresponding to this lexical context, and is used to figure
 * out what module prefixes are required to properly qualify a type.
 *
 * Raises: Failure if the children of this node contain a cycle. *)
let rec generate_struct_node nodes_table scope struct_def =
  let unsorted_fields =
    let fields_accessor = PS.Node.Struct.fields_get struct_def in
    let rec loop_fields acc i =
      if i = CArray.length fields_accessor then
        acc
      else
        let field = CArray.get fields_accessor i in
        loop_fields (field :: acc) (i + 1)
    in
    loop_fields [] 0
  in
  (* Sorting in reverse code order allows us to avoid a List.rev *)
  let all_fields = List.sort unsorted_fields ~cmp:(fun x y ->
    - (Int.compare (PS.Field.codeOrder_get x) (PS.Field.codeOrder_get y)))
  in
  let union_fields, non_union_fields = List.partition_tf all_fields ~f:(fun field ->
    (PS.Field.discriminantValue_get field) <> no_discriminant)
  in
  let union_accessors =
    match union_fields with
    | [] -> ""
    | _  -> generate_union_accessors nodes_table scope struct_def union_fields
  in
  let non_union_acccessors =
    match non_union_fields with
    | [] -> ""
    | _  -> generate_non_union_accessors nodes_table scope struct_def non_union_fields
  in
  union_accessors ^ non_union_acccessors



(* Generate the OCaml module corresponding to a node.  [scope] is a stack of
 * scope IDs corresponding to this lexical context, and is used to figure out
 * what module prefixes are required to properly qualify a type.
 *
 * Raises: Failure if the children of this node contain a cycle. *)
and generate_node
    (nodes_table : (Uint64.t, Message.ro PS.Node.t) Hashtbl.t)
    (scope : Uint64.t list)
    (node : Message.ro PS.Node.t)
    (node_name : string)
: string =
  let node_id = PS.Node.id_get node in
  let indent = String.concat (List.rev_map scope ~f:(fun x -> "  ")) in
  let header = Printf.sprintf "%smodule %s = struct\n" indent node_name in
  let footer = indent ^ "end\n" in
  let accessors =
    match PS.Node.unnamed_union_get node with
    | PS.Node.File ->
        ""
    | PS.Node.Struct struct_def ->
        generate_struct_node nodes_table scope struct_def
    |_ ->
        ""
  in
  match Topsort.topological_sort nodes_table (children_of nodes_table node) with
  | Some child_nodes ->
      let child_modules = List.map child_nodes ~f:(fun child ->
        let child_name = get_unqualified_name ~parent:node ~child in
        generate_node nodes_table (node_id :: scope) child child_name)
      in
      begin match scope with
      | [_] | [] ->
          String.concat ~sep:"\n" child_modules
      | x :: _ ->
          String.concat ~sep:"\n" (header :: (child_modules @ [accessors ^ footer]))
      end
  | None ->
      let error_msg = Printf.sprintf
        "The children of node %s (%s) have a cyclic dependency."
        (Uint64.to_string node_id)
        (PS.Node.displayName_get node)
      in
      failwith error_msg


let compile (request : Message.ro PS.CodeGeneratorRequest.t) (dest_dir : string) : unit =
  let nodes_table = Hashtbl.Poly.create () in
  let nodes = PS.CodeGeneratorRequest.nodes_get request in
  for i = 0 to CArray.length nodes - 1 do
    let node = CArray.get nodes i in
    Hashtbl.replace nodes_table ~key:(PS.Node.id_get node) ~data:node
  done;
  let requested_files = PS.CodeGeneratorRequest.requestedFiles_get request in
  for i = 0 to CArray.length requested_files - 1 do
    let requested_file = CArray.get requested_files i in
    let open PS.CodeGeneratorRequest in
    let requested_file_id = RequestedFile.id_get requested_file in
    let requested_file_node = Hashtbl.find_exn nodes_table requested_file_id in
    let requested_filename = RequestedFile.filename_get requested_file_node in
    let file_content = generate_node nodes_table [requested_file_id]
      requested_file_node requested_filename
    in
    print_endline file_content
  done


