
open Core.Std

module PS = PluginSchema.Make(StrStorage)


let add_parentage_maps
    (nodes_table : (Uint64.t, Message.ro PS.Node.t) Hashtbl.t)
    (parentage_table : (Uint64.t, Uint64.t) Hashtbl.t)
    (node : Message.ro PS.Node.t)
: unit =
  let node_id = PS.Node.id_get node in
  let rec add_children parent =
    let child_nodes = PS.Node.nestedNodes_get parent in
    for i = 0 to PS.List.length child_nodes - 1 do
      let child_nested_node = PS.List.get child_nodes i in
      let child_node = Hashtbl.find_exn nodes_table (PS.Node.NestedNode.id_get child_nested_node) in
      let child_node_id = PS.Node.id_get child_node in
      let () = add_children child_node in
      Hashtbl.replace parentage_table ~key:child_node_id ~data:node_id
    done
  in
  let () = add_children node in
  (* Also adding an identity map for the parent node *)
  Hashtbl.replace parentage_table ~key:node_id ~data:node_id


let build_parentage_table
    (nodes_table : (Uint64.t, Message.ro PS.Node.t) Hashtbl.t)
    (nodes : Message.ro PS.Node.t list)
: (Uint64.t, Uint64.t) Hashtbl.t =
  let parentage_table = Hashtbl.Poly.create () in
  let () =
    List.iter nodes ~f:(fun node -> add_parentage_maps nodes_table parentage_table node)
  in
  parentage_table


let register_reference ~parentage_table ~edges ~referrer ~referee : unit =
  match Hashtbl.find parentage_table referee with
  | Some parent_referee ->
      if Util.uint64_equal parent_referee referrer then
        (* This would be be a reference from a child node to one of its grandparents, or
         * a reference between two child nodes.  In the first case, this reference is
         * not important for the purpose of topological sorting; in the second case,
         * this implies a topological sorting of the child nodes which will be sorted
         * out on a later pass. *)
        ()
      else
        Hashtbl.add_multi edges ~key:parent_referee ~data:referrer
  | None ->
      (* When recursing within node M, we may find reference to nodes which are not contained
       * within node M.  These references will not be contained in the parentage table,
       * and are not important for the purpose of topological sorting. *)
      ()


let rec register_type_reference
    ~parentage_table
    ~edges
    ~referrer
    ~referee_type:(tp : Message.ro PS.Type.t)
: unit =
  match PS.Type.unnamed_union_get tp with
  | PS.Type.List x ->
      let inner_type = PS.Type.List.elementType_get x in
      register_type_reference ~parentage_table ~edges ~referrer ~referee_type:inner_type
  | PS.Type.Enum x ->
      register_reference ~parentage_table ~edges ~referrer
        ~referee:(PS.Type.Enum.typeId_get x)
  | PS.Type.Struct x ->
      register_reference ~parentage_table ~edges ~referrer
        ~referee:(PS.Type.Struct.typeId_get x)
  | PS.Type.Interface x ->
      register_reference ~parentage_table ~edges ~referrer
        ~referee:(PS.Type.Interface.typeId_get x)
  | _ ->
      ()


(* Generate a table which contains a map from node A to node B iff
 * B or any of B's children references A or any of A's children... i.e. if
 * the generated code for node A must be instantiated prior to the generated
 * code for node B. *)
let build_reference_graph
    (nodes_table : (Uint64.t, Message.ro PS.Node.t) Hashtbl.t)
    (nodes_to_graph : Message.ro PS.Node.t list)
: (Uint64.t, Uint64.t list) Hashtbl.t =
  let rec add_edges ~parentage_table ~edges ?parent_id_opt node =
    (* While iterating through a node's children, we create edges from the *parent*
     * and not from the child.  [parent_id] will always record the toplevel
     * node ID regardless of how deep we recurse. *)
    let parent_id =
      match parent_id_opt with
      | None -> (* i.e. current node is toplevel *)
          PS.Node.id_get node
      | Some id ->
          id
    in
    let () =
      let child_nodes = PS.Node.nestedNodes_get node in
      for i = 0 to PS.List.length child_nodes - 1 do
        let child_nested_node = PS.List.get child_nodes i in
        let child_node = Hashtbl.find_exn nodes_table (PS.Node.NestedNode.id_get child_nested_node) in
        add_edges ~parentage_table ~edges ~parent_id_opt:parent_id child_node;
      done
    in
    match PS.Node.unnamed_union_get node with
    | PS.Node.File
    | PS.Node.Enum _
    | PS.Node.Annotation _ ->
        (* Annotations are (typically) not reflected directly in the generated code,
         * so at least for the present we ignore annotation types when determining
         * the order in which to generate code. *)
        ()
    | PS.Node.Struct node_struct ->
        let fields = PS.Node.Struct.fields_get node_struct in
        for j = 0 to PS.List.length fields - 1 do
          let field = PS.List.get fields j in
          match PS.Field.unnamed_union_get field with
          | PS.Field.Slot slot ->
              register_type_reference ~parentage_table ~edges
                ~referrer:parent_id ~referee_type:(PS.Field.Slot.type_get slot)
          | PS.Field.Group group ->
              let group_node = Hashtbl.find_exn nodes_table (PS.Field.Group.typeId_get group) in
              add_edges ~parentage_table ~edges ~parent_id_opt:parent_id group_node
        done
    | PS.Node.Interface node_iface ->
        let methods = PS.Node.Interface.methods_get node_iface in
        for j = 0 to PS.List.length methods - 1 do
          let meth = PS.List.get methods j in
          register_reference ~parentage_table ~edges
            ~referrer:parent_id ~referee:(PS.Method.paramStructType_get meth);
          register_reference ~parentage_table ~edges
            ~referrer:parent_id ~referee:(PS.Method.resultStructType_get meth)
        done
    | PS.Node.Const node_const ->
        register_type_reference ~parentage_table ~edges
          ~referrer:parent_id ~referee_type:(PS.Node.Const.type_get node_const)
  in
  let parentage_table = build_parentage_table nodes_table nodes_to_graph in
  let edges = Hashtbl.Poly.create () in
  let () = List.iter nodes_to_graph ~f:(fun node -> add_edges ~parentage_table ~edges node) in
  edges


let dump_reference_graph reference_graph =
  let () = Printf.printf "reference graph:\n" in
  Hashtbl.iter reference_graph ~f:(fun ~key ~data ->
    let () = Printf.printf "  key: %s\n" (Uint64.to_string key) in
    List.iter data ~f:(fun x -> Printf.printf "    data: %s\n" (Uint64.to_string x)))


let has_incoming_edges reference_graph (node_id : Uint64.t) : bool =
  Hashtbl.exists reference_graph ~f:(fun referee_node_ids ->
    List.mem ~equal:Util.uint64_equal referee_node_ids node_id)


(* Sort a list of nodes in such a way that the generated ocaml modules will be
 * declared prior to the point of use.
 *
 * Returns None if there are cyclic references. *)
let topological_sort
    (nodes_table : (Uint64.t, Message.ro PS.Node.t) Hashtbl.t)
    (nodes : Message.ro PS.Node.t list)
: Message.ro PS.Node.t list option =
  (* [priority_nodes] is a list of nodes without any incoming edges.  Such a node
   * can be emitted immediately, because it doesn't depend on anything else. *)
  let rec kahn_sort ~reference_graph ~sorted_output_ids ~priority_node_ids =
    match priority_node_ids with
    | [] ->
        sorted_output_ids
    | priority_node_id :: other_priority_node_ids ->
        begin match Hashtbl.find reference_graph priority_node_id with
        | Some referrers ->
            let () = Hashtbl.remove reference_graph priority_node_id in
            let new_priority_node_ids = List.fold_left referrers
              ~init:other_priority_node_ids
              ~f:(fun acc x ->
                if has_incoming_edges reference_graph x then
                  acc
                else
                  x :: acc)
            in
            kahn_sort ~reference_graph
              ~sorted_output_ids:(priority_node_id :: sorted_output_ids)
              ~priority_node_ids:new_priority_node_ids
        | None ->
            kahn_sort ~reference_graph
              ~sorted_output_ids:(priority_node_id :: sorted_output_ids)
              ~priority_node_ids:other_priority_node_ids
        end
  in
  let node_ids = List.map nodes ~f:PS.Node.id_get in
  let reference_graph = build_reference_graph nodes_table nodes in
  let priority_node_ids = List.filter node_ids ~f:(fun id ->
    not (has_incoming_edges reference_graph id))
  in
  let rev_sorted_node_ids = kahn_sort ~reference_graph ~sorted_output_ids:[] ~priority_node_ids in
  if Hashtbl.is_empty reference_graph then
    Some (List.rev_map rev_sorted_node_ids ~f:(Hashtbl.find_exn nodes_table))
  else
    None


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
  match topological_sort nodes_table (children_of nodes_table node) with
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


