
open Core.Std

module PS = PluginSchema.Make(StrStorage)
module CArray = PS.CapnpArray


let no_discriminant = 0xffff


(* Generate a function for unpacking a capnp union type as an OCaml variant. *)
let generate_union_accessors nodes_table scope struct_def fields =
  let indent = String.make (2 * (List.length scope + 1)) ' ' in
  (GenCommon.generate_union_type nodes_table scope struct_def fields) ^ "\n" ^
  (Printf.sprintf "%sval unnamed_union_get : t -> unnamed_union_t\n" indent)


(* Generate accessors for retrieving all non-union fields of a struct. *)
let generate_non_union_accessors nodes_table scope struct_def fields =
  let indent = String.make (2 * (List.length scope + 1)) ' ' in
  let accessors = List.fold_left fields ~init:[] ~f:(fun acc field ->
    let field_accessors : string =
      let field_name = PS.Field.name_get field in
      match PS.Field.unnamed_union_get field with
      | PS.Field.Group group ->
          let group_id = PS.Field.Group.typeId_get group in
          let group_node = Hashtbl.find_exn nodes_table group_id in
          let group_name = GenCommon.get_scope_relative_name nodes_table scope group_node in
          Printf.sprintf "%sval %s_get : t -> %s.t\n" indent field_name group_name
      | PS.Field.Slot slot ->
          let tp = PS.Field.Slot.type_get slot in
          begin match PS.Type.unnamed_union_get tp with
          | PS.Type.Int32 ->
              String.concat ~sep:"" [
                Printf.sprintf "%sval %s_get : t -> int32\n" indent field_name;
                Printf.sprintf "%sval %s_get_int_exn : t -> int\n" indent field_name;
              ]
          | PS.Type.Int64 ->
              String.concat ~sep:"" [
                Printf.sprintf "%sval %s_get : t -> int64\n" indent field_name;
                Printf.sprintf "%sval %s_get_int_exn : t -> int\n" indent field_name;
              ]
          | PS.Type.Uint32 ->
              String.concat ~sep:"" [
                Printf.sprintf "%sval %s_get : t -> Uint32.t\n" indent field_name;
                Printf.sprintf "%sval %s_get_int_exn : t -> int\n" indent field_name;
              ]
          | PS.Type.Uint64 ->
              String.concat ~sep:"" [
                Printf.sprintf "%sval %s_get : t -> Uint64.t\n" indent field_name;
                Printf.sprintf "%sval %s_get_int_exn : t -> int\n" indent field_name;
              ]
          | PS.Type.Void
          | PS.Type.Bool
          | PS.Type.Int8
          | PS.Type.Int16
          | PS.Type.Uint8
          | PS.Type.Uint16
          | PS.Type.Float32
          | PS.Type.Float64
          | PS.Type.Text
          | PS.Type.Data
          | PS.Type.List _
          | PS.Type.Enum _
          | PS.Type.Struct _
          | PS.Type.Interface _
          | PS.Type.Object ->
              Printf.sprintf "%sval %s_get : t -> %s\n"
              indent
              field_name
              (GenCommon.type_name nodes_table scope tp)
          end
    in
    (field_accessors :: acc))
  in
  String.concat ~sep:"" accessors


(* Generate the OCaml type signature corresponding to a struct definition.  [scope] is a
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



(* Generate the OCaml type signature corresponding to a node.  [scope] is
 * a stack of scope IDs corresponding to this lexical context, and is used to figure out
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
  let indent = String.make (2 * (List.length scope)) ' ' in
  let header = Printf.sprintf "%smodule %s : sig\n" indent node_name in
  let footer = indent ^ "end\n" in
  let body =
    match PS.Node.unnamed_union_get node with
    | PS.Node.File ->
        ""
    | PS.Node.Struct struct_def ->
        generate_struct_node nodes_table scope struct_def
    | PS.Node.Enum enum_def ->
        GenCommon.generate_enum_sig ~nodes_table ~scope ~enum_node:enum_def
    |_ ->
        ""
  in
  match Topsort.topological_sort nodes_table (GenCommon.children_of nodes_table node) with
  | Some child_nodes ->
      let child_modules = List.map child_nodes ~f:(fun child ->
        let child_name = GenCommon.get_unqualified_name ~parent:node ~child in
        generate_node nodes_table (node_id :: scope) child child_name)
      in
      begin match scope with
      | [] ->
          String.concat ~sep:"\n" child_modules
      | _ ->
          let nested = List.fold_left child_modules ~init:"" ~f:(fun acc x ->
            acc ^ x ^ "\n")
          in
          header ^ nested ^ body ^ footer
      end
  | None ->
      let error_msg = Printf.sprintf
        "The children of node %s (%s) have a cyclic dependency."
        (Uint64.to_string node_id)
        (PS.Node.displayName_get node)
      in
      failwith error_msg


