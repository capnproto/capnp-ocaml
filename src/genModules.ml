
open Core.Std

module PS = PluginSchema.Make(StrStorage)
module CArray = PS.CapnpArray


let no_discriminant = 0xffff


(* Generate a function for unpacking a capnp union type as an OCaml variant. *)
let generate_union_accessors nodes_table scope struct_def fields =
  let indent = String.make (2 * (List.length scope + 1)) ' ' in
  let cases = List.fold_left fields ~init:[] ~f:(fun acc field ->
    let field_name = String.capitalize (PS.Field.name_get field) in
    let field_decoder =
      (* FIXME: decode default values here *)
      match PS.Field.unnamed_union_get field with
      | PS.Field.Slot slot ->
          let ofs = Uint32.to_int (PS.Field.Slot.offset_get slot) in
          let tp  = PS.Field.Slot.type_get slot in
          begin match PS.Type.unnamed_union_get tp with
          | PS.Type.Void ->
              ""
          | PS.Type.Bool ->
              Printf.sprintf " (get_struct_field_bit x %u %u)" (ofs / 8) (ofs mod 8)
          | PS.Type.Int8 ->
              Printf.sprintf " (get_struct_field_int8 x %u)" ofs
          | PS.Type.Int16 ->
              Printf.sprintf " (get_struct_field_int16 x %u)" (ofs * 2)
          | PS.Type.Int32 ->
              Printf.sprintf " (get_struct_field_int32 x %u)" (ofs * 4)
          | PS.Type.Int64 ->
              Printf.sprintf " (get_struct_field_int64 x %u)" (ofs * 8)
          | PS.Type.Uint8 ->
              Printf.sprintf " (get_struct_field_uint8 x %u)" ofs
          | PS.Type.Uint16 ->
              Printf.sprintf " (get_struct_field_uint16 x %u)" (ofs * 2)
          | PS.Type.Uint32 ->
              Printf.sprintf " (get_struct_field_uint32 x %u)" (ofs * 4)
          | PS.Type.Uint64 ->
              Printf.sprintf " (get_struct_field_uint64 x %u)" (ofs * 8)
          | PS.Type.Float32 ->
              Printf.sprintf " (Int32.float_of_bits (get_struct_field_int32 x %u))" (ofs * 4)
          | PS.Type.Float64 ->
              Printf.sprintf " (Int64.float_of_bits (get_struct_field_int64 x %u))" (ofs * 8)
          | PS.Type.Text ->
              Printf.sprintf " (get_struct_field_text x %u)" (ofs * 8)
          | PS.Type.Data ->
              Printf.sprintf " (get_struct_field_blob x %u)" (ofs * 8)
          | PS.Type.List _ ->
              failwith "list type unexpected in union field"
          | PS.Type.Enum _ ->
              failwith "enum type unexpected in union field"
          | PS.Type.Struct _ ->
              failwith "struct type unexpected in union field"
          | PS.Type.Interface _ ->
              failwith "interface type unexpected in union field"
          | PS.Type.Object ->
              Printf.sprintf " (get_struct_pointer x %u)" (ofs * 8)
          end
      | PS.Field.Group group ->
          (* Groups are just a different view of the storage associated with the parent node *)
          " x"
    in
    let field_value = PS.Field.discriminantValue_get field in
    (Printf.sprintf "%s  | %u -> %s%s"
      indent
      field_value
      field_name
      field_decoder) :: acc)
  in
  let header = [
    Printf.sprintf "%slet unnamed_union_get x =" indent;
    Printf.sprintf "%s  match get_struct_field_uint16 x %u with"
      indent ((Uint32.to_int (PS.Node.Struct.discriminantOffset_get struct_def)) * 2);
  ] in
  let footer = [
    Printf.sprintf "%s  | v -> Undefined_ v\n" indent
  ] in
  (GenCommon.generate_union_type nodes_table scope struct_def fields) ^ "\n" ^
  String.concat ~sep:"\n" (header @ cases @ footer)


(* Generate an accessor for retrieving a list of the given type. *)
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


(* Generate an accessor for decoding an enum type. *)
let generate_enum_accessor ~nodes_table ~scope ~enum_node ~enum_type ~indent ~field_name ~field_ofs =
  let header =
    Printf.sprintf "%slet %s_get x = \n%s  match get_struct_field_uint16 x %u with\n"
      indent
      field_name
      indent
      (2 * field_ofs)
  in
  let match_cases =
    let scope_relative_name = GenCommon.get_scope_relative_name nodes_table scope enum_node in
    let enumerants =
      match PS.Node.unnamed_union_get enum_node with
      | PS.Node.Enum enum_group ->
          PS.Node.Enum.enumerants_get enum_group
      | _ ->
          failwith "decoded non-enum node where enum node was expected"
    in
    let buf = Buffer.create 512 in
    for i = 0 to CArray.length enumerants - 1 do
      let enumerant = CArray.get enumerants i in
      let match_case =
        Printf.sprintf "%s  | %u -> %s.%s\n"
          indent
          i
          scope_relative_name
          (String.capitalize (PS.Enumerant.name_get enumerant))
      in
      Buffer.add_string buf match_case
    done;
    let footer = Printf.sprintf "%s  | v -> %s.Undefined_ v\n" indent scope_relative_name in
    let () = Buffer.add_string buf footer in
    Buffer.contents buf
  in
  header ^ match_cases


(* Generate accessors for retrieving all non-union fields of a struct. *)
let generate_non_union_accessors nodes_table scope struct_def fields =
  let indent = String.make (2 * (List.length scope + 1)) ' ' in
  let accessors = List.fold_left fields ~init:[] ~f:(fun acc field ->
    let field_accessors : string =
      let field_name = String.uncapitalize (PS.Field.name_get field) in
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
              String.concat ~sep:"" [
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
              String.concat ~sep:"" [
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
              String.concat ~sep:"" [
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
              String.concat ~sep:"" [
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
              let enum_id = PS.Type.Enum.typeId_get enum in
              let enum_node = Hashtbl.find_exn nodes_table enum_id in
              generate_enum_accessor ~nodes_table ~scope ~enum_node
                ~enum_type:tp ~indent ~field_name ~field_ofs
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
  String.concat ~sep:"" accessors


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
  let indent = String.make (2 * (List.length scope + 1)) ' ' in
  (Printf.sprintf "%stype t = Message.ro StructStorage.t option\n\n" indent) ^
  union_accessors ^ non_union_acccessors



(* Generate the OCaml module and type signature corresponding to a node.  [scope] is
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
  let header = Printf.sprintf "%smodule %s = struct\n" indent node_name in
  let footer = indent ^ "end\n" in
  let accessors =
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
          header ^ nested ^ accessors ^ footer
      end
  | None ->
      let error_msg = Printf.sprintf
        "The children of node %s (%s) have a cyclic dependency."
        (Uint64.to_string node_id)
        (PS.Node.displayName_get node)
      in
      failwith error_msg


