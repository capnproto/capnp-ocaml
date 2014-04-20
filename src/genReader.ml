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
module Mode = GenCommon.Mode
module R  = Runtime
module Reader = MessageReader.Make(GenCommon.M)

let sprintf = Printf.sprintf
let apply_indent = GenCommon.apply_indent


(* Generate a decoder lambda for converting from a uint16 to the associated enum value. *)
let generate_enum_decoder_lines ~nodes_table ~scope ~enum_node ~indent =
  let header = [ "(fun u16 -> match u16 with" ] in
  let scope_relative_name =
    GenCommon.get_scope_relative_name nodes_table scope enum_node
  in
  let match_cases =
    let enumerants =
      match PS.Node.unnamed_union_get enum_node with
      | PS.Node.Enum enum_group ->
          PS.Node.Enum.enumerants_get enum_group
      | _ ->
          failwith "Decoded non-enum node where enum node was expected."
    in
    let rec loop_cases acc i =
      if i = R.Array.length enumerants then
        List.rev acc
      else
        let enumerant = R.Array.get enumerants i in
        let case_str =
          sprintf "  | %u -> %s.%s" i scope_relative_name
            (String.capitalize (PS.Enumerant.name_get enumerant))
        in
        loop_cases (case_str :: acc) (i + 1)
    in
    loop_cases [] 0
  in
  let footer = [ sprintf "  | v -> %s.Undefined_ v)" scope_relative_name ] in
  apply_indent ~indent (header @ match_cases @ footer)


(* Generate a decoder lambda for converting from a uint16 to the associated enum value. *)
(* FIXME: switch to generate_enum_decoder_lines *)
let generate_enum_decoder ~nodes_table ~scope ~enum_node ~indent ~field_ofs =
  let lines = generate_enum_decoder_lines ~nodes_table ~scope ~enum_node ~indent in
  (String.concat ~sep:"\n" lines) ^ "\n"


(* Generate an accessor for decoding an enum type. *)
let generate_enum_accessor ~nodes_table ~scope ~enum_node ~indent ~field_name
    ~field_ofs ~default =
  let lines = [
    "let " ^ field_name ^ "_get x =";
    "  let decode =";
  ] @ (generate_enum_decoder_lines ~nodes_table ~scope ~enum_node
      ~indent:(indent ^ "    ")) @ [
    "  in";
    sprintf "  let discr = get_data_field x \
             ~f:(get_uint16 ~default:%u ~byte_ofs:%u) in"
      default (field_ofs * 2);
    "  decode discr";
  ] in
  apply_indent ~indent lines


(* There is no get_enum() or get_enum_list() in the runtime API,
   because the enum values are schema-dependent.  This function
   will generate something appropriate for localized use. *)
let generate_enum_runtime_getters ~nodes_table ~scope ~indent enum_def =
  let enum_id = PS.Type.Enum.typeId_get enum_def in
  let enum_node = Hashtbl.find_exn nodes_table enum_id in
  let decoder_lambda =
    (generate_enum_decoder_lines ~nodes_table ~scope ~enum_node
        ~indent:(indent ^ "    "))
  in
  let lines = [
    "let get_enum ~byte_ofs data_opt =";
    "  let decode ="; ] @ decoder_lambda @ [
    "  in";
    "  decode (get_uint16 ~default:0 ~byte_ofs data_opt)";
    "let get_enum_list ~default pointer_opt =";
    "  let decode ="; ] @ decoder_lambda @ [
    "  in";
    "  get_list ~default (ListDecoders.Bytes2 (fun slice ->";
    "    decode (Slice.get_uint16 slice 0)))";
    "    pointer_opt";
  ] in
  apply_indent ~indent lines


let rec generate_list_element_decoder ~nodes_table ~scope ~list_def ~indent =
  let make_terminal_decoder element_name = apply_indent ~indent [
      "let decoders = ListDecoders.Pointer (fun slice ->";
      "  get_" ^ element_name ^ "_list ~default:(make_empty_array ()) (Some slice))";
      "in";
    ]
  in
  let contained_type = PS.Type.List.elementType_get list_def in
  match PS.Type.unnamed_union_get contained_type with
  | PS.Type.Void     -> make_terminal_decoder "void"
  | PS.Type.Bool     -> make_terminal_decoder "bit"
  | PS.Type.Int8     -> make_terminal_decoder "int8"
  | PS.Type.Int16    -> make_terminal_decoder "int16"
  | PS.Type.Int32    -> make_terminal_decoder "int32"
  | PS.Type.Int64    -> make_terminal_decoder "int64"
  | PS.Type.Uint8    -> make_terminal_decoder "uint8"
  | PS.Type.Uint16   -> make_terminal_decoder "uint16"
  | PS.Type.Uint32   -> make_terminal_decoder "uint32"
  | PS.Type.Uint64   -> make_terminal_decoder "uint64"
  | PS.Type.Float32  -> make_terminal_decoder "float32"
  | PS.Type.Float64  -> make_terminal_decoder "float64"
  | PS.Type.Text     -> make_terminal_decoder "text"
  | PS.Type.Data     -> make_terminal_decoder "blob"
  | PS.Type.Struct _ -> make_terminal_decoder "struct"
  | PS.Type.List inner_list_def ->
      let inner_decoder_decl = generate_list_element_decoder ~nodes_table
         ~scope ~list_def:inner_list_def ~indent:(indent ^ "  ")
      in
      apply_indent ~indent [
        "let decoders = ListDecoders.Pointer (fun slice ->";
      ] @ inner_decoder_decl @ [
        "  get_list ~default:(make_empty_array ()) decoders (Some slice))";
      ]
  | PS.Type.Enum enum_def ->
      let enum_getters =
        generate_enum_runtime_getters ~nodes_table ~scope
          ~indent:(indent ^ "  ") enum_def
      in
      let lines = [
        "let decoders =";
      ] @ enum_getters @ [
        "  ListDecoders.Pointer (fun slice ->";
        "    get_enum_list ~default:(make_empty_array ()) (Some slice))";
        "in";
      ] in
      apply_indent ~indent lines
  | PS.Type.Interface _ ->
      failwith "not implemented"
  | PS.Type.AnyPointer ->
      failwith "not implemented"
  | PS.Type.Undefined_ x ->
       failwith (sprintf "Unknown Type union discriminant %d" x)


(* Generate an accessor for retrieving a list of the given type. *)
let generate_list_accessor ~nodes_table ~scope ~list_type ~indent
    ~field_name ~field_ofs =
  let make_primitive_accessor element_name =
    apply_indent ~indent [
      "let " ^ field_name ^ "_get x =";
      sprintf "  get_pointer_field x %u ~f:(get_%s_list \
               ~default:(make_empty_array ()))"
        field_ofs element_name;
    ]
  in
  match PS.Type.unnamed_union_get list_type with
  | PS.Type.Void     -> make_primitive_accessor "void"
  | PS.Type.Bool     -> make_primitive_accessor "bit"
  | PS.Type.Int8     -> make_primitive_accessor "int8"
  | PS.Type.Int16    -> make_primitive_accessor "int16"
  | PS.Type.Int32    -> make_primitive_accessor "int32"
  | PS.Type.Int64    -> make_primitive_accessor "int64"
  | PS.Type.Uint8    -> make_primitive_accessor "uint8"
  | PS.Type.Uint16   -> make_primitive_accessor "uint16"
  | PS.Type.Uint32   -> make_primitive_accessor "uint32"
  | PS.Type.Uint64   -> make_primitive_accessor "uint64"
  | PS.Type.Float32  -> make_primitive_accessor "float32"
  | PS.Type.Float64  -> make_primitive_accessor "float64"
  | PS.Type.Text     -> make_primitive_accessor "text"
  | PS.Type.Data     -> make_primitive_accessor "blob"
  | PS.Type.Struct _ -> make_primitive_accessor "struct"
  | PS.Type.List list_def ->
      let decoder_declaration = generate_list_element_decoder
          ~nodes_table ~scope ~list_def ~indent:(indent ^ "  ")
      in
      let lines = [
        "let " ^ field_name ^ "_get x =";
      ] @ decoder_declaration @ [
        sprintf "  get_pointer_field x %u ~f:(get_list \
                 ~default:(make_empty_array ()) decoders)"
          field_ofs;
      ] in
      apply_indent ~indent lines
  | PS.Type.Enum enum_def ->
      let enum_id = PS.Type.Enum.typeId_get enum_def in
      let enum_node = Hashtbl.find_exn nodes_table enum_id in
      let decoder_declaration =
        (generate_enum_decoder_lines ~nodes_table ~scope ~enum_node
            ~indent:(indent ^ "    "))
      in
      let lines = [
        "let " ^ field_name ^ "_get x =";
        "  let enum_decoder ="; ] @ decoder_declaration @ [
        "  in";
        "  get_pointer_field x %u f:(get_list ~default:(make_empty_array ())";
        "    decoders:(ListDecoders.Bytes2 enum_decoder))";
        ] in
      apply_indent ~indent lines
  | PS.Type.Interface _ ->
      apply_indent ~indent [
        "let " ^ field_name ^ "_get x = failwith \"not implemented (type iface)\"";
      ]
  | PS.Type.AnyPointer ->
      apply_indent ~indent [
        "let " ^ field_name ^ "_get x = failwith \"not implemented (type anyptr)\"";
      ]
  | PS.Type.Undefined_ x ->
       failwith (sprintf "Unknown Type union discriminant %d" x)


(* FIXME: would be nice to unify default value logic with [generate_constant]... *)
let generate_field_accessor ~nodes_table ~scope ~indent field =
  let field_name = String.uncapitalize (PS.Field.name_get field) in
  match PS.Field.unnamed_union_get field with
  | PS.Field.Group group ->
      [ indent ^ "let " ^ field_name ^ "_get x = x" ]
  | PS.Field.Slot slot ->
      let field_ofs = Uint32.to_int (PS.Field.Slot.offset_get slot) in
      let tp = PS.Field.Slot.type_get slot in
      let default = PS.Field.Slot.defaultValue_get slot in
      begin match (PS.Type.unnamed_union_get tp, PS.Value.unnamed_union_get default) with
      | (PS.Type.Void, PS.Value.Void) ->
          [ indent ^ "let " ^ field_name ^ "_get x = ()" ]
      | (PS.Type.Bool, PS.Value.Bool a) ->
          apply_indent ~indent [
            "let " ^ field_name ^ "_get x =";
            sprintf "  get_data_field x ~f:(get_bit ~default:%s ~byte_ofs:%u \
                     ~bit_ofs:%u)"
              (if a then "true" else "false")
              (field_ofs / 8)
              (field_ofs mod 8);
          ]
      | (PS.Type.Int8, PS.Value.Int8 a) ->
          apply_indent ~indent [
            "let " ^ field_name ^ "_get x =";
            sprintf "  get_data_field x ~f:(get_int8 ~default:%d ~byte_ofs:%u)"
              a field_ofs;
          ]
      | (PS.Type.Int16, PS.Value.Int16 a) ->
          apply_indent ~indent [
            "let " ^ field_name ^ "_get x =";
            sprintf "  get_data_field x ~f:(get_int16 ~default:%d ~byte_ofs:%u)"
              a (field_ofs * 2)
          ]
      | (PS.Type.Int32, PS.Value.Int32 a) ->
          apply_indent ~indent [
            "let " ^ field_name ^ "_get x =";
            sprintf "  get_data_field x ~f:(get_int32 ~default:%sl ~byte_ofs:%u)"
              (Int32.to_string a) (field_ofs * 4);
            "let " ^ field_name ^ "_get_int_exn x =";
            "  Int32.to_int (" ^ field_name ^ "_get x)";
          ]
      | (PS.Type.Int64, PS.Value.Int64 a) ->
          apply_indent ~indent [
            "let " ^ field_name ^ "_get x =";
            sprintf "  get_data_field x ~f:(get_int64 ~default:%sL ~byte_ofs:%u)"
              (Int64.to_string a) (field_ofs * 8);
            "let " ^ field_name ^ "_get_int_exn x =";
            "  Int64.to_int (" ^ field_name ^ "_get x)";
          ]
      | (PS.Type.Uint8, PS.Value.Uint8 a) ->
          apply_indent ~indent [
            "let " ^ field_name ^ "_get x =";
            sprintf "  get_data_field x ~f:(get_uint8 ~default:%d ~byte_ofs:%u)"
              a field_ofs;
          ]
      | (PS.Type.Uint16, PS.Value.Uint16 a) ->
          apply_indent ~indent [
            "let " ^ field_name ^ "_get x =";
            sprintf "  get_data_field x ~f:(get_uint16 ~default:%d ~byte_ofs:%u)"
              a (field_ofs * 2)
          ]
      | (PS.Type.Uint32, PS.Value.Uint32 a) ->
          let default =
            if Uint32.compare a Uint32.zero = 0 then
              "Uint32.zero"
            else
              sprintf "(Uint32.of_string \"%s\")" (Uint32.to_string a)
          in
          apply_indent ~indent [
            "let " ^ field_name ^ "_get x =";
            sprintf "  get_data_field x ~f:(get_uint32 ~default:%s ~byte_ofs:%u)"
              default (field_ofs * 4);
            "let " ^ field_name ^ "_get_int_exn x =";
            "  Uint32.to_int (" ^ field_name ^ "_get x)";
          ]
      | (PS.Type.Uint64, PS.Value.Uint64 a) ->
          let default =
            if Uint64.compare a Uint64.zero = 0 then
              "Uint64.zero"
            else
              sprintf "(Uint64.of_string \"%s\")" (Uint64.to_string a)
          in
          apply_indent ~indent [
            "let " ^ field_name ^ "_get x =";
            sprintf "  get_data_field x ~f:(get_uint64 ~default:%s ~byte_ofs:%u)"
              default (field_ofs * 8);
            "let " ^ field_name ^ "_get_int_exn x =";
            "  Uint64.to_int (" ^ field_name ^ "_get x)";
          ]
      | (PS.Type.Float32, PS.Value.Float32 a) ->
          apply_indent ~indent [
            "let " ^ field_name ^ "_get x =";
            sprintf "  get_data_field x ~f:(get_float32 ~default_bits:%sl ~byte_ofs:%u)"
              (Int32.to_string (Int32.bits_of_float a))
              (field_ofs * 4);
          ]
      | (PS.Type.Float64, PS.Value.Float64 a) ->
          apply_indent ~indent [
            "let " ^ field_name ^ "_get x =";
            sprintf "  get_data_field x ~f:(get_float64 ~default_bits:%sL ~byte_ofs:%u)"
              (Int64.to_string (Int64.bits_of_float a))
              (field_ofs * 8);
          ]
      | (PS.Type.Text, PS.Value.Text a) ->
          apply_indent ~indent [
            "let " ^ field_name ^ "_get x =";
            sprintf "  get_pointer_field x %u ~f:(get_text ~default:\"%s\")"
              field_ofs (String.escaped a);
          ]
      | (PS.Type.Data, PS.Value.Data a) ->
          apply_indent ~indent [
            "let " ^ field_name ^ "_get x =";
            sprintf "  get_pointer_field x %u ~f:(get_blob ~default:\"%s\")"
              field_ofs (String.escaped a);
          ]
      | (PS.Type.List list_def, PS.Value.List pointer_slice_opt) ->
          let has_trivial_default =
            begin match pointer_slice_opt with
            | Some pointer_slice ->
                begin match Reader.decode_pointer pointer_slice with
                | Pointer.Null -> true
                | _ -> false
                end
            | None ->
                true
            end
          in
          if has_trivial_default then
            let list_type = PS.Type.List.elementType_get list_def in
            generate_list_accessor ~nodes_table ~scope ~list_type ~indent
              ~field_name ~field_ofs
          else
            failwith "Default values for lists are not implemented."
      | (PS.Type.Enum enum_def, PS.Value.Enum val_uint16) ->
          let enum_id = PS.Type.Enum.typeId_get enum_def in
          let enum_node = Hashtbl.find_exn nodes_table enum_id in
          generate_enum_accessor
            ~nodes_table ~scope ~enum_node ~indent ~field_name ~field_ofs
            ~default:val_uint16
      | (PS.Type.Struct struct_def, PS.Value.Struct pointer_slice_opt) ->
          let has_trivial_default =
            begin match pointer_slice_opt with
            | Some pointer_slice ->
                begin match Reader.decode_pointer pointer_slice with
                | Pointer.Null -> true
                | _ -> false
                end
            | None ->
                true
            end
          in
          if has_trivial_default then
            apply_indent ~indent [
              "let " ^ field_name ^ "_get x =";
              sprintf "  get_pointer_field x %u ~f:(get_struct ~default:None)"
                field_ofs;
            ]
          else
              failwith "Default values for structs are not implemented."
      | (PS.Type.Interface iface_def, PS.Value.Interface) ->
          [ indent ^ "let " ^ field_name ^ "_get x = failwith \"not implemented (iface 2)\"" ]
      | (PS.Type.AnyPointer, PS.Value.AnyPointer pointer) ->
          apply_indent indent [
            "let " ^ field_name ^ "_get x =";
            sprintf "  get_pointer_field x %u ~f:(fun x -> x)" field_ofs;
          ]
      | (PS.Type.Undefined_ x, _) ->
          failwith (sprintf "Unknown Field union discriminant %u." x)

      (* All other cases represent an ill-formed default value in the plugin request *)
      | (PS.Type.Void, _)
      | (PS.Type.Bool, _)
      | (PS.Type.Int8, _)
      | (PS.Type.Int16, _)
      | (PS.Type.Int32, _)
      | (PS.Type.Int64, _)
      | (PS.Type.Uint8, _)
      | (PS.Type.Uint16, _)
      | (PS.Type.Uint32, _)
      | (PS.Type.Uint64, _)
      | (PS.Type.Float32, _)
      | (PS.Type.Float64, _)
      | (PS.Type.Text, _)
      | (PS.Type.Data, _)
      | (PS.Type.List _, _)
      | (PS.Type.Enum _, _)
      | (PS.Type.Struct _, _)
      | (PS.Type.Interface _, _)
      | (PS.Type.AnyPointer, _) ->
          let err_msg = sprintf
              "The default value for field \"%s\" has an unexpected type."
              field_name
          in
          failwith err_msg
      end
  | PS.Field.Undefined_ x ->
      failwith (sprintf "Unknown Field union discriminant %u." x)


(* Generate a function for unpacking a capnp union type as an OCaml variant. *)
let generate_union_accessor_lines ~nodes_table ~scope struct_def fields =
  let indent = String.make (2 * (List.length scope + 2)) ' ' in
  let cases = List.fold_left fields ~init:[] ~f:(fun acc field ->
    let field_name = String.uncapitalize (PS.Field.name_get field) in
    let ctor_name = String.capitalize field_name in
    let field_value = PS.Field.discriminantValue_get field in
    let field_has_void_type =
      match PS.Field.unnamed_union_get field with
      | PS.Field.Slot slot ->
          begin match PS.Type.unnamed_union_get (PS.Field.Slot.type_get slot) with
          | PS.Type.Void -> true
          | _ -> false
          end
      | _ -> false
    in
    if field_has_void_type then
      (sprintf "%s  | %u -> %s"
        indent
        field_value
        ctor_name) :: acc
    else
      (sprintf "%s  | %u -> %s (%s_get x)"
        indent
        field_value
        ctor_name
        field_name) :: acc)
  in
  let header = apply_indent ~indent [
      "let unnamed_union_get x =";
      sprintf "  match get_data_field x \
               ~f:(get_uint16 ~default:0 ~byte_ofs:%u) with"
        ((Uint32.to_int (PS.Node.Struct.discriminantOffset_get struct_def)) * 2);
    ] in
  let footer = [ indent ^ "  | v -> Undefined_ v" ] in
  (GenCommon.generate_union_type_lines ~mode:Mode.Reader nodes_table scope
     struct_def fields) @ header @ cases @ footer


(* FIXME: get rid of this *)
let generate_union_accessor ~nodes_table ~scope struct_def fields =
  (String.concat ~sep:"\n"
     (generate_union_accessor_lines ~nodes_table ~scope struct_def fields)) ^ "\n"


(* Generate accessors for retrieving all fields of a struct, regardless of whether
 * or not the fields are packed into a union.  (Fields packed inside a union are
 * not exposed in the module signature. *)
let generate_accessors ~nodes_table ~scope struct_def fields =
  let indent = String.make (2 * (List.length scope + 2)) ' ' in
  List.fold_left fields ~init:[] ~f:(fun acc field ->
    let x = generate_field_accessor ~nodes_table ~scope ~indent field in
    x @ acc)


(* Generate the OCaml module corresponding to a struct definition.  [scope] is a
 * stack of scope IDs corresponding to this lexical context, and is used to figure
 * out what module prefixes are required to properly qualify a type.
 *
 * Raises: Failure if the children of this node contain a cycle. *)
let rec generate_struct_node ~nodes_table ~scope ~nested_modules ~node struct_def =
  let unsorted_fields =
    let fields_accessor = PS.Node.Struct.fields_get struct_def in
    let rec loop_fields acc i =
      if i = R.Array.length fields_accessor then
        acc
      else
        let field = R.Array.get fields_accessor i in
        loop_fields (field :: acc) (i + 1)
    in
    loop_fields [] 0
  in
  (* Sorting in reverse code order allows us to avoid a List.rev *)
  let all_fields = List.sort unsorted_fields ~cmp:(fun x y ->
    - (Int.compare (PS.Field.codeOrder_get x) (PS.Field.codeOrder_get y)))
  in
  let union_fields = List.filter all_fields ~f:(fun field ->
    (PS.Field.discriminantValue_get field) <> PS.Field.noDiscriminant)
  in
  let accessors : string list = generate_accessors ~nodes_table ~scope struct_def all_fields in
  let union_accessors : string list =
    match union_fields with
    | [] -> []
    | _  -> generate_union_accessor_lines ~nodes_table ~scope struct_def union_fields
  in
  let indent = String.make (2 * (List.length scope + 2)) ' ' in
  let unique_reader = (GenCommon.make_unique_typename ~mode:Mode.Reader
      ~scope_mode:Mode.Reader ~nodes_table node)
  in
  let unique_builder = (GenCommon.make_unique_typename ~mode:Mode.Builder
      ~scope_mode:Mode.Reader ~nodes_table node)
  in
  let types =
    apply_indent ~indent [
      "type t = ro StructStorage.t option";
      "type " ^ unique_reader ^ " = t";
      "type builder_t = rw StructStorage.t";
      "type " ^ unique_builder ^ " = builder_t";
      "type array_t = ro ListStorage.t";
    ]
  in
  types @ nested_modules @ accessors @ union_accessors @
    [ indent ^ "let of_message x = get_root_struct x" ]


(* Generate the OCaml module and type signature corresponding to a node.  [scope] is
 * a stack of scope IDs corresponding to this lexical context, and is used to figure out
 * what module prefixes are required to properly qualify a type.
 *
 * Raises: Failure if the children of this node contain a cycle. *)
and generate_node
    ~(suppress_module_wrapper : bool)
    ~(nodes_table : (Uint64.t, PS.Node.t) Hashtbl.t)
    ~(scope : Uint64.t list)
    ~(node_name : string)
    (node : PS.Node.t)
: string list =
  let node_id = PS.Node.id_get node in
  let indent = String.make (2 * (List.length scope + 1)) ' ' in
  let generate_nested_modules () =
    match Topsort.topological_sort nodes_table
            (GenCommon.children_of nodes_table node) with
    | Some child_nodes ->
        List.concat_map child_nodes ~f:(fun child ->
          let child_name = GenCommon.get_unqualified_name ~parent:node ~child in
          let child_node_id = PS.Node.id_get child in
          generate_node ~suppress_module_wrapper:false ~nodes_table
            ~scope:(child_node_id :: scope) ~node_name:child_name child)
    | None ->
        let error_msg = sprintf
          "The children of node %s (%s) have a cyclic dependency."
          (Uint64.to_string node_id)
          (PS.Node.displayName_get node)
        in
        failwith error_msg
  in
  match PS.Node.unnamed_union_get node with
  | PS.Node.File ->
      generate_nested_modules ()
  | PS.Node.Struct struct_def ->
      let nested_modules = generate_nested_modules () in
      let body =
        generate_struct_node ~nodes_table ~scope ~nested_modules ~node struct_def
      in
      if suppress_module_wrapper then
        body
      else
        [ indent ^ "module " ^ node_name ^ " = struct" ] @ body @
          [ indent ^ "end" ]
  | PS.Node.Enum enum_def ->
      let nested_modules = generate_nested_modules () in
      let body =
        GenCommon.generate_enum_sig_lines ~nodes_table ~scope ~nested_modules
          (* FIXME *)
          ~mode:GenCommon.Mode.Reader
          ~node enum_def
      in
      if suppress_module_wrapper then
        body
      else
        [ indent ^ "module " ^ node_name ^ " = struct" ] @
        body @
        [ indent ^ "end" ]
  | PS.Node.Interface iface_def ->
      generate_nested_modules ()
  | PS.Node.Const const_def ->
      apply_indent ~indent [
        "let " ^ (String.uncapitalize node_name) ^ " = " ^
          (GenCommon.generate_constant ~nodes_table ~scope const_def);
      ]
  | PS.Node.Annotation annot_def ->
      generate_nested_modules ()
  | PS.Node.Undefined_ x ->
      failwith (sprintf "Unknown Node union discriminant %u" x)

