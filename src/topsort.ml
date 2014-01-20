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

module PS = GenCommon.PS
module R  = Runtime


let add_parentage_maps
    (nodes_table : (Uint64.t, PS.Node.t) Hashtbl.t)
    (parentage_table : (Uint64.t, Uint64.t) Hashtbl.t)
    (node : PS.Node.t)
: unit =
  let node_id = PS.Node.id_get node in
  let rec add_children parent =
    let child_nodes = PS.Node.nestedNodes_get parent in
    for i = 0 to R.Array.length child_nodes - 1 do
      let child_nested_node = R.Array.get child_nodes i in
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
    (nodes_table : (Uint64.t, PS.Node.t) Hashtbl.t)
    (nodes : PS.Node.t list)
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
    ~referee_type:(tp : PS.Type.t)
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
    (nodes_table : (Uint64.t, PS.Node.t) Hashtbl.t)
    (nodes_to_graph : PS.Node.t list)
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
      for i = 0 to R.Array.length child_nodes - 1 do
        let child_nested_node = R.Array.get child_nodes i in
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
        for j = 0 to R.Array.length fields - 1 do
          let field = R.Array.get fields j in
          match PS.Field.unnamed_union_get field with
          | PS.Field.Slot slot ->
              register_type_reference ~parentage_table ~edges
                ~referrer:parent_id ~referee_type:(PS.Field.Slot.type_get slot)
          | PS.Field.Group group ->
              let group_node = Hashtbl.find_exn nodes_table (PS.Field.Group.typeId_get group) in
              add_edges ~parentage_table ~edges ~parent_id_opt:parent_id group_node
          | PS.Field.Undefined_ x ->
              failwith (Printf.sprintf "Unknown Field union discriminant %d" x)
        done
    | PS.Node.Interface node_iface ->
        let methods = PS.Node.Interface.methods_get node_iface in
        for j = 0 to R.Array.length methods - 1 do
          let meth = R.Array.get methods j in
          register_reference ~parentage_table ~edges
            ~referrer:parent_id ~referee:(PS.Method.paramStructType_get meth);
          register_reference ~parentage_table ~edges
            ~referrer:parent_id ~referee:(PS.Method.resultStructType_get meth)
        done
    | PS.Node.Const node_const ->
        register_type_reference ~parentage_table ~edges
          ~referrer:parent_id ~referee_type:(PS.Node.Const.type_get node_const)
    | PS.Node.Undefined_ x ->
        failwith (Printf.sprintf "Unknown Node union discriminant %d" x)
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
    (nodes_table : (Uint64.t, PS.Node.t) Hashtbl.t)
    (nodes : PS.Node.t list)
: PS.Node.t list option =
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


