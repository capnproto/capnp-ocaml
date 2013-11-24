
open Core.Std

module PS = PluginSchema.Make(StrStorage)


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
    if i = PS.List.length nested_nodes then
      None
    else
      let nested_node = PS.List.get nested_nodes i in
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
            if i = PS.List.length fields then
              failwith error_msg
            else
             let field = PS.List.get fields i in
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


(* Generate the OCaml module corresponding to a node.  [scope] is a stack of
 * scope IDs corresponding to this lexical context, and is used to figure out
 * what module prefixes are required to properly qualify a type.
 *
 * Raises: Failure if the children of this node contain a cycle. *)
let rec generate_node
    (nodes_table : (Uint64.t, Message.ro PS.Node.t) Hashtbl.t)
    (scope : Uint64.t list)
    (node : Message.ro PS.Node.t)
    (node_name : string)
: string =
  let node_id = PS.Node.id_get node in
  let indent = String.concat (List.rev_map scope ~f:(fun x -> "  ")) in
  let header = Printf.sprintf "%smodule %s = struct\n" indent node_name in
  let footer = indent ^ "end\n" in
  match Topsort.topological_sort nodes_table (children_of nodes_table node) with
  | Some child_nodes ->
      let child_modules = List.map child_nodes ~f:(fun child ->
        let child_name = get_unqualified_name ~parent:node ~child in
        generate_node nodes_table (node_id :: scope) child child_name)
      in
      String.concat ~sep:"\n" (header :: (child_modules @ [footer]))
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
  for i = 0 to PS.List.length nodes - 1 do
    let node = PS.List.get nodes i in
    Hashtbl.replace nodes_table ~key:(PS.Node.id_get node) ~data:node
  done;
  let requested_files = PS.CodeGeneratorRequest.requestedFiles_get request in
  for i = 0 to PS.List.length requested_files - 1 do
    let requested_file = PS.List.get requested_files i in
    let open PS.CodeGeneratorRequest in
    let requested_file_id = RequestedFile.id_get requested_file in
    let requested_file_node = Hashtbl.find_exn nodes_table requested_file_id in
    let requested_filename = RequestedFile.filename_get requested_file_node in
    let file_content = generate_node nodes_table [] requested_file_node requested_filename in
    print_endline file_content

  done


