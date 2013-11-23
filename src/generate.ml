
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


let register_reference ~parentage_table ~edges ~edge_from_node_id (id : Uint64.t) : unit =
  let parent_reference = Hashtbl.find_exn parentage_table id in
  Hashtbl.add_multi edges ~key:edge_from_node_id ~data:parent_reference


let rec register_type_reference
    ~parentage_table
    ~edges
    ~edge_from_node_id
    (tp : Message.ro PS.Type.t)
: unit =
  match PS.Type.unnamed_union_get tp with
  | PS.Type.List x ->
      let inner_type = PS.Type.List.elementType_get x in
      register_type_reference ~parentage_table ~edges ~edge_from_node_id inner_type
  | PS.Type.Enum x ->
      register_reference ~parentage_table ~edges ~edge_from_node_id
        (PS.Type.Enum.typeId_get x)
  | PS.Type.Struct x ->
      register_reference ~parentage_table ~edges ~edge_from_node_id
        (PS.Type.Struct.typeId_get x)
  | PS.Type.Interface x ->
      register_reference ~parentage_table ~edges ~edge_from_node_id
        (PS.Type.Interface.typeId_get x)
  | _ ->
      ()


(* Generate a table which contains a map from node A to node B iff
 * B or any of B's children references A or any of A's children. *)
let build_graph
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
                ~edge_from_node_id:parent_id (PS.Field.Slot.type_get slot)
          | PS.Field.Group group ->
              register_reference ~parentage_table ~edges
                ~edge_from_node_id:parent_id (PS.Field.Group.typeId_get group)
        done
    | PS.Node.Interface node_iface ->
        let methods = PS.Node.Interface.methods_get node_iface in
        for j = 0 to PS.List.length methods - 1 do
          let meth = PS.List.get methods j in
          register_reference ~parentage_table ~edges
            ~edge_from_node_id:parent_id (PS.Method.paramStructType_get meth);
          register_reference ~parentage_table ~edges
            ~edge_from_node_id:parent_id (PS.Method.resultStructType_get meth)
        done
    | PS.Node.Const node_const ->
        register_type_reference ~parentage_table ~edges
          ~edge_from_node_id:parent_id (PS.Node.Const.type_get node_const)
  in
  let parentage_table = build_parentage_table nodes_table nodes_to_graph in
  let edges = Hashtbl.Poly.create () in
  let () = List.iter nodes_to_graph ~f:(fun node -> add_edges ~parentage_table ~edges node) in
  edges



let compile (request : Message.ro PS.CodeGeneratorRequest.t) (dest_dir : string) =
  let node_table = Hashtbl.Poly.create () in
  let nodes = PS.CodeGeneratorRequest.nodes_get request in
  for i = 0 to PS.List.length nodes - 1 do
    let node = PS.List.get nodes i in
    Hashtbl.replace node_table ~key:(PS.Node.id_get node) ~data:node
  done;
  let toplevel_nodes = Hashtbl.fold node_table ~init:[] ~f:(fun ~key:id ~data:node acc ->
    if Uint64.compare (PS.Node.scopeId_get node) Uint64.zero = 0 then
      node :: acc
    else
      acc)
  in
  List.iter toplevel_nodes ~f:(fun node ->
    Printf.printf "toplevel node: %s\n"
      (Uint64.to_string (PS.Node.id_get node)))


  (* TODO:
   *
   * Form a tree to represent the structure of the generated code.  The leaves of the tree
   * need to be topologically sorted so that OCaml identifiers will be correctly declared prior to
   * the point of use.
   *
   * Once the sorting is done, code generation can be accomplished using a relatively
   * straightforward recursive (depth-first) tree descent.
   *)


