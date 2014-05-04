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

module PS        = GenCommon.PS
module Mode      = GenCommon.Mode
module C         = Capnp
module ReaderApi = C.Runtime.Reader.Make(GenCommon.M)

let sprintf = Printf.sprintf
let apply_indent = GenCommon.apply_indent


let api_of_mode mode =
  match mode with
  | Mode.Reader  -> "RA_"
  | Mode.Builder -> "BA_"


(* Generate a decoder lambda for converting from a uint16 to the
   associated enum value. *)
let generate_enum_decoder ~nodes_table ~scope ~enum_node =
  let header = [ "(fun u16 -> match u16 with" ] in
  let scope_relative_name =
    GenCommon.get_scope_relative_name nodes_table scope enum_node
  in
  let enumerants =
    match PS.Node.get enum_node with
    | PS.Node.Enum enum_group ->
        PS.Node.Enum.enumerants_get enum_group
    | _ ->
        failwith "Decoded non-enum node where enum node was expected."
  in
  let match_cases =
    C.Array.foldi_right enumerants ~init:[] ~f:(fun i enumerant acc ->
      let case_str =
        sprintf "  | %u -> %s.%s" i scope_relative_name
          (String.capitalize (PS.Enumerant.name_get enumerant))
      in
      case_str :: acc)
  in
  let undefined_name = GenCommon.mangle_enum_undefined enumerants in
  let footer = [
    sprintf "  | v -> %s.%s v)" scope_relative_name undefined_name
  ] in
  header @ match_cases @ footer


(* Generate an encoder lambda for converting from an enum value to the associated
   uint16.  [allow_undefined] indicates whether or not to permit enum values which
   use the [Undefined] constructor. *)
let generate_enum_encoder ~(allow_undefined : bool) ~nodes_table ~scope
    ~enum_node =
  let header = [ "(fun enum -> match enum with" ] in
  let scope_relative_name =
    GenCommon.get_scope_relative_name nodes_table scope enum_node
  in
  let enumerants =
    match PS.Node.get enum_node with
    | PS.Node.Enum enum_group ->
        PS.Node.Enum.enumerants_get enum_group
    | _ ->
        failwith "Decoded non-enum node where enum node was expected."
  in
  let match_cases =
    C.Array.foldi_right enumerants ~init:[] ~f:(fun i enumerant acc ->
      let case_str =
        sprintf "  | %s.%s -> %u"
          scope_relative_name
          (String.capitalize (PS.Enumerant.name_get enumerant))
          i
      in
      case_str :: acc)
  in
  let footer = 
    let undefined_name = GenCommon.mangle_enum_undefined enumerants in
    if allow_undefined then [
      sprintf "  | %s.%s x -> x)" scope_relative_name undefined_name
    ] else [
      sprintf "  | %s.%s _ ->" scope_relative_name undefined_name;
              "      invalid_msg \"Cannot encode undefined enum value.\")";
    ]
  in
  header @ match_cases @ footer


(* Generate an accessor for decoding an enum type. *)
let generate_enum_getter ~nodes_table ~scope ~enum_node ~mode
    ~field_name ~field_ofs ~default =
  let api_module = api_of_mode mode in
  let decoder_lambda =
    apply_indent ~indent:"    "
      (generate_enum_decoder ~nodes_table ~scope ~enum_node)
  in [
    "let " ^ field_name ^ "_get x =";
    "  let decode ="; ] @ decoder_lambda @ [
    "  in";
    sprintf "  let discr = %s.get_data_field x \
             ~f:(%s.get_uint16 ~default:%u ~byte_ofs:%u) in"
      api_module
      api_module
      default
      (field_ofs * 2);
    "  decode discr";
  ]


(* Generate an accessor for setting the value of an enum. *)
let generate_enum_safe_setter ~nodes_table ~scope ~enum_node ~field_name
    ~field_ofs ~default ~discr_str =
  let encoder_lambda =
    apply_indent ~indent:"    "
      (generate_enum_encoder ~allow_undefined:false
         ~nodes_table ~scope ~enum_node)
  in [
    "let " ^ field_name ^ "_set x e =";
    "  let encode =" ] @ encoder_lambda @ [
    "  in";
    sprintf
      "  BA_.get_data_field %sx ~f:(BA_.set_uint16 \
       ~default:%u ~byte_ofs:%u (encode e))"
      discr_str
      default
      (field_ofs * 2);
  ]


(* Generate an accessor for setting the value of an enum, permitting values
   which are not defined in the schema. *)
let generate_enum_unsafe_setter ~nodes_table ~scope ~enum_node
    ~field_name ~field_ofs ~default ~discr_str =
  let encoder_lambda =
    apply_indent ~indent:"    "
      (generate_enum_encoder ~allow_undefined:true
        ~nodes_table ~scope ~enum_node)
  in [
    "let " ^ field_name ^ "_set_unsafe x e =";
    "  let encode =" ] @ encoder_lambda @ [
    "  in";
    sprintf
      "  BA_.get_data_field %sx ~f:(BA_.set_uint16 \
       ~default:%u ~byte_ofs:%u (encode e))"
      discr_str
      default
      (field_ofs * 2);
  ]


(* There is no get_enum() or get_enum_list() in the runtime API,
   because the enum values are schema-dependent.  This function
   will generate something appropriate for localized use. *)
let generate_enum_runtime_getters ~nodes_table ~scope ~mode enum_def =
  let api_module = api_of_mode mode in
  let enum_id = PS.Type.Enum.type_id_get enum_def in
  let enum_node = Hashtbl.find_exn nodes_table enum_id in
  let decoder_lambda =
    apply_indent ~indent:"    "
      (generate_enum_decoder ~nodes_table ~scope ~enum_node)
  in
  let encoder_lambda =
    apply_indent ~indent:"    "
      (generate_enum_encoder ~nodes_table ~scope ~enum_node
         ~allow_undefined:false)
  in
  let get_enum_list =
    match mode with
    | Mode.Reader -> [
        "let get_enum_list ?default pointer_opt =";
        "  let decode ="; ] @ decoder_lambda @ [
        "  in";
        "  RA_.get_list ?default (RA_.ListDecoders.Bytes2 (fun slice ->";
        "    decode (Slice.get_uint16 slice 0)))";
        "    pointer_opt";
        "in";
      ]
    | Mode.Builder -> [
        "let get_enum_list ?default pointer =";
        "  let decode ="; ] @ decoder_lambda @ [
        "  in";
        "  let encode ="; ] @ encoder_lambda @ [
        "  in";
        "  let codecs = BA_.ListCodecs.Bytes2 (";
        "    (fun slice -> decode (Slice.get_uint16 slice 0)),";
        "    (fun v slice -> Slice.set_uint16 slice 0 (encode v)))";
        "  in";
        "  BA_.get_list ~storage_type:BA_.ListStorage.Bytes2 ~codecs pointer_bytes";
        "in";
      ]
  in [
    "let get_enum ~byte_ofs data_opt =";
    "  let decode ="; ] @ decoder_lambda @ [
    "  in";
    "  decode (" ^ api_module ^ ".get_uint16 ~default:0 ~byte_ofs data_opt)";
    "in";
  ] @ get_enum_list


(* There is no set_enum() or set_enum_list() in the runtime API,
   because the enum values are schema-dependent.  This function
   will generate something appropriate for localized use. *)
let generate_enum_runtime_setters ~nodes_table ~scope enum_def =
  let enum_id = PS.Type.Enum.type_id_get enum_def in
  let enum_node = Hashtbl.find_exn nodes_table enum_id in
  let decoder_lambda =
    apply_indent ~indent:"    "
      (generate_enum_decoder ~nodes_table ~scope ~enum_node)
  in
  let encoder_lambda =
    apply_indent ~indent:"    "
      (generate_enum_encoder ~nodes_table ~scope ~enum_node
         ~allow_undefined:false)
  in [
    "let set_enum ~byte_ofs value data =";
    "  let encode ="; ] @ encoder_lambda @ [
    "  in";
    "  BA_.set_uint16 ~default:0 ~byte_ofs (encode value) data";
    "in";
    "let set_enum_list ~default value pointer_bytes =";
    "  let decode ="; ] @ decoder_lambda @ [
    "  in";
    "  let encode ="; ] @ encoder_lambda @ [
    "  in";
    "  let codecs = BA_.ListCodecs.Bytes2 (";
    "    (fun slice -> decode (Slice.get_uint16 slice 0)),";
    "    (fun v slice -> Slice.set_uint16 slice 0 (encode v)))";
    "  in";
    "  BA_.set_list ~storage_type:BA_.ListStorage.Bytes2 ~codecs value pointer_bytes";
    "in";
  ]


(* Generate a set of decoder functions for reading elements from a Cap'n Proto
   List<T>.  The resulting decoders could be passed as an argument to
   [Reader.get_list] in order to generate a getter for a list field. *)
let rec generate_list_element_decoder ~nodes_table ~scope list_def =
  let make_terminal_decoder element_name = [
      "let decoders = RA_.ListDecoders.Pointer (fun slice ->";
      "  RA_.get_" ^ element_name ^ "_list (Some slice))";
      "in";
    ]
  in
  let open PS.Type in
  let contained_type = List.element_type_get list_def in
  match get contained_type with
  | Void     -> make_terminal_decoder "void"
  | Bool     -> make_terminal_decoder "bit"
  | Int8     -> make_terminal_decoder "int8"
  | Int16    -> make_terminal_decoder "int16"
  | Int32    -> make_terminal_decoder "int32"
  | Int64    -> make_terminal_decoder "int64"
  | Uint8    -> make_terminal_decoder "uint8"
  | Uint16   -> make_terminal_decoder "uint16"
  | Uint32   -> make_terminal_decoder "uint32"
  | Uint64   -> make_terminal_decoder "uint64"
  | Float32  -> make_terminal_decoder "float32"
  | Float64  -> make_terminal_decoder "float64"
  | Text     -> make_terminal_decoder "text"
  | Data     -> make_terminal_decoder "blob"
  | Struct _ -> make_terminal_decoder "struct"
  | List inner_list_def ->
      let inner_decoder_decl =
        apply_indent ~indent:"  "
          (generate_list_element_decoder ~nodes_table ~scope inner_list_def)
      in [
        "let decoders = RA_.ListDecoders.Pointer (fun slice ->";
      ] @ inner_decoder_decl @ [
        "  RA_.get_list decoders (Some slice))";
      ]
  | Enum enum_def ->
      let enum_getters =
        apply_indent ~indent:"  "
          (generate_enum_runtime_getters ~nodes_table ~scope
             ~mode:Mode.Reader enum_def)
      in [
        "let decoders =";
      ] @ enum_getters @ [
        "  RA_.ListDecoders.Pointer (fun slice ->";
        "    get_enum_list (Some slice))";
        "in";
      ]
  | Interface _ ->
      failwith "not implemented"
  | AnyPointer ->
      failwith "not implemented"
  | Undefined x ->
       failwith (sprintf "Unknown Type union discriminant %d" x)


(* Generate a set of decoder/encoder functions for reading and writing
   elements from/to a Cap'n Proto List<T>.  The resulting codecs could
   be passed as an argument to [Builder.get_list] in order to generate
   a getter for a list field. *)
let rec generate_list_element_codecs ~nodes_table ~scope list_def =
  let make_terminal_codecs element_name = [
      "let codecs =";
      "  let decode slice = BA_.get_" ^ element_name ^ "_list slice in";
      "  let encode v slice = BA_.set_" ^ element_name ^ "_list v slice in";
      "  BA_.ListCodecs.Pointer (decode, encode)";
      "in";
    ]
  in
  let open PS.Type in
  let contained_type = List.element_type_get list_def in
  match get contained_type with
  | Void     -> make_terminal_codecs "void"
  | Bool     -> make_terminal_codecs "bit"
  | Int8     -> make_terminal_codecs "int8"
  | Int16    -> make_terminal_codecs "int16"
  | Int32    -> make_terminal_codecs "int32"
  | Int64    -> make_terminal_codecs "int64"
  | Uint8    -> make_terminal_codecs "uint8"
  | Uint16   -> make_terminal_codecs "uint16"
  | Uint32   -> make_terminal_codecs "uint32"
  | Uint64   -> make_terminal_codecs "uint64"
  | Float32  -> make_terminal_codecs "float32"
  | Float64  -> make_terminal_codecs "float64"
  | Text     -> make_terminal_codecs "text"
  | Data     -> make_terminal_codecs "blob"
  | Struct _ -> make_terminal_codecs "struct"
  | List inner_list_def ->
      let inner_codecs_decl =
        apply_indent ~indent:"  "
          (generate_list_element_codecs ~nodes_table ~scope inner_list_def)
      in [
        "let codecs ="; ] @ inner_codecs_decl @ [
        "  let decode slice = BA_.get_list ~codecs slice in";
        "  let encode v slice = BA_.set_list ~codecs v slice in";
        "  BA_.ListCodecs.Pointer (decode, encode)";
        "in";
      ]
  | Enum enum_def ->
      let enum_getters =
        apply_indent ~indent:"  "
          (generate_enum_runtime_getters ~nodes_table ~scope
             ~mode:Mode.Builder enum_def)
      in
      let enum_setters =
        apply_indent ~indent:"  "
          (generate_enum_runtime_setters ~nodes_table ~scope enum_def)
      in [
        "let codecs ="; ] @ enum_getters @ enum_setters @ [
        "  RA_.ListDecoders.Pointer (get_enum_list, set_enum_list)";
        "in";
      ]
  | Interface _ ->
      failwith "not implemented"
  | AnyPointer ->
      failwith "not implemented"
  | Undefined x ->
       failwith (sprintf "Unknown Type union discriminant %d" x)


(* Generate an accessor for retrieving a list of the given type. *)
let generate_list_getter ~nodes_table ~scope ~list_type ~mode
    ~field_name ~field_ofs =
  let api_module = api_of_mode mode in
  let make_primitive_accessor element_name = [
      "let " ^ field_name ^ "_get x =";
      sprintf "  %s.get_pointer_field x %u ~f:%s.get_%s_list"
        api_module
        field_ofs
        api_module
        element_name;
    ]
  in
  let open PS.Type in
  match get list_type with
  | Void     -> make_primitive_accessor "void"
  | Bool     -> make_primitive_accessor "bit"
  | Int8     -> make_primitive_accessor "int8"
  | Int16    -> make_primitive_accessor "int16"
  | Int32    -> make_primitive_accessor "int32"
  | Int64    -> make_primitive_accessor "int64"
  | Uint8    -> make_primitive_accessor "uint8"
  | Uint16   -> make_primitive_accessor "uint16"
  | Uint32   -> make_primitive_accessor "uint32"
  | Uint64   -> make_primitive_accessor "uint64"
  | Float32  -> make_primitive_accessor "float32"
  | Float64  -> make_primitive_accessor "float64"
  | Text     -> make_primitive_accessor "text"
  | Data     -> make_primitive_accessor "blob"
  | Struct struct_def ->
      begin match mode with
      | Mode.Reader -> [
          "let " ^ field_name ^ "_get x =";
          sprintf "  RA_.get_pointer_field x %u ~f:RA_.get_struct_list"
            field_ofs;
        ]
      | Mode.Builder ->
          let data_words, pointer_words =
            let id = PS.Type.Struct.type_id_get struct_def in
            let node = Hashtbl.find_exn nodes_table id in
            match PS.Node.get node with
            | PS.Node.Struct struct_def ->
                (PS.Node.Struct.data_word_count_get struct_def,
                 PS.Node.Struct.pointer_count_get struct_def)
            | _ ->
                failwith
                  "Decoded non-struct node where struct node was expected."
          in [
            "let " ^ field_name ^ "_get x =";
            sprintf "  BA_.get_pointer_field x %u \
                     ~f:(BA_.get_struct_list ~data_words:%u ~pointer_words:%u)"
              field_ofs
              data_words
              pointer_words;
          ]
      end
  | List list_def ->
      let decoder_declaration =
        apply_indent ~indent:"  "
          (generate_list_element_decoder ~nodes_table ~scope list_def)
      in [
        "let " ^ field_name ^ "_get x =";
      ] @ decoder_declaration @ [
        sprintf "  %s.get_pointer_field x %u ~f:(%s.get_list decoders)"
          api_module
          field_ofs
          api_module;
      ]
  | Enum enum_def ->
      let enum_id = Enum.type_id_get enum_def in
      let enum_node = Hashtbl.find_exn nodes_table enum_id in
      let decoder_lambda =
        apply_indent ~indent:"  "
          (generate_enum_decoder ~nodes_table ~scope ~enum_node)
      in [
        "let " ^ field_name ^ "_get x =";
        "  let enum_decoder ="; ] @ decoder_lambda @ [
        "  in";
        sprintf "  %s.get_pointer_field x %u f:(%s.get_list "
          api_module field_ofs api_module;
        "    (RA_.ListDecoders.Bytes2 enum_decoder))";
        ]
  | Interface _ -> [
        "let " ^ field_name ^ "_get x = failwith \"not implemented (type iface)\"";
      ]
  | AnyPointer -> [
        "let " ^ field_name ^ "_get x = failwith \"not implemented (type anyptr)\"";
      ]
  | Undefined x ->
       failwith (sprintf "Unknown Type union discriminant %d" x)


(* Generate accessors for setting or initializing a list of the given type. *)
let generate_list_setters ~nodes_table ~scope ~list_type
    ~discr_str ~field_name ~field_ofs =
  let make_primitive_setters element_name = [
    "let " ^ field_name ^ "_set x v =";
    sprintf "  BA_.get_pointer_field %sx %u ~f:(BA_.set_%s_list v)"
      discr_str
      field_ofs
      element_name;
    "let " ^ field_name ^ "_init x n =";
    sprintf "  BA_.get_pointer_field %sx %u ~f:(BA_.init_%s_list n)"
      discr_str
      field_ofs
      element_name;
  ] in
  let open PS.Type in
  match get list_type with
  | Void     -> make_primitive_setters "void"
  | Bool     -> make_primitive_setters "bit"
  | Int8     -> make_primitive_setters "int8"
  | Int16    -> make_primitive_setters "int16"
  | Int32    -> make_primitive_setters "int32"
  | Int64    -> make_primitive_setters "int64"
  | Uint8    -> make_primitive_setters "uint8"
  | Uint16   -> make_primitive_setters "uint16"
  | Uint32   -> make_primitive_setters "uint32"
  | Uint64   -> make_primitive_setters "uint64"
  | Float32  -> make_primitive_setters "float32"
  | Float64  -> make_primitive_setters "float64"
  | Text     -> make_primitive_setters "text"
  | Data     -> make_primitive_setters "blob"
  | Struct struct_def ->
      let data_words, pointer_words =
        let id = PS.Type.Struct.type_id_get struct_def in
        let node = Hashtbl.find_exn nodes_table id in
        match PS.Node.get node with
        | PS.Node.Struct struct_def ->
            (PS.Node.Struct.data_word_count_get struct_def,
             PS.Node.Struct.pointer_count_get struct_def)
        | _ ->
            failwith
              "Decoded non-struct node where struct node was expected."
      in [
        "let " ^ field_name ^ "_set x v =";
        sprintf "  BA_.get_pointer_field %sx %u \
                 ~f:(BA_.set_struct_list ~data_words:%u ~pointer_words:%u v)"
          discr_str
          field_ofs
          data_words
          pointer_words;
        "let " ^ field_name ^ "_init x n =";
        sprintf "  BA_.get_pointer_field %sx %u \
                 ~f:(BA_.init_struct_list ~data_words:%u ~pointer_words:%u n)"
          discr_str
          field_ofs
          data_words
          pointer_words;
      ]
  | List list_def ->
      let codecs_declaration =
        apply_indent ~indent:"  "
          (generate_list_element_codecs ~nodes_table ~scope list_def)
      in [
        "let " ^ field_name ^ "_set x v =";
      ] @ codecs_declaration @ [
        sprintf "  BA_.get_pointer_field %sx %u ~f:(BA_.set_list ~codecs v)"
          discr_str field_ofs;
        "let " ^ field_name ^ "_init x n =";
      ] @ codecs_declaration @ [
        sprintf "  BA_.get_pointer_field %sx %u \
                  ~f:(BA_.init_list ~storage_type:BA_.ListStorage.Pointer \
                  ~codecs n)"
          discr_str field_ofs;
      ]
  | Enum enum_def ->
      let enum_id = Enum.type_id_get enum_def in
      let enum_node = Hashtbl.find_exn nodes_table enum_id in
      let decoder_lambda =
        apply_indent ~indent:"    "
          (generate_enum_decoder ~nodes_table ~scope ~enum_node)
      in
      let encoder_lambda =
        apply_indent ~indent:"    "
          (generate_enum_encoder ~nodes_table ~scope ~enum_node
             ~allow_undefined:false)
      in [
        "let " ^ field_name ^ "_set x v =";
        "  let enum_decoder ="; ] @ decoder_lambda @ [
        "  in";
        "  let enum_encoder ="; ] @ encoder_lambda @ [
        "  in";
        sprintf "  BA_.get_pointer_field %sx %u f:(BA_.set_list "
          discr_str field_ofs;
        "    ~codecs:(BA_.ListCodecs.Bytes2 (enum_decoder, enum_encoder)))";
        "let " ^ field_name ^ "_init x n =";
        "  let enum_decoder ="; ] @ decoder_lambda @ [
        "  in";
        "  let enum_encoder ="; ] @ encoder_lambda @ [
        "  in";
        sprintf "  BA_.get_pointer_field %sx %u f:(BA_.init_list "
          discr_str field_ofs;
        "    ~storage_type:BA_.ListStorage.Bytes2 \
         ~codecs:(BA_.ListCodecs.Bytes2 (enum_decoder, enum_encoder)))";
      ]
  | Interface _ -> [
        "let " ^ field_name ^ "_get x = failwith \"not implemented (type iface)\"";
      ]
  | AnyPointer -> [
        "let " ^ field_name ^ "_get x = failwith \"not implemented (type anyptr)\"";
      ]
  | Undefined x ->
       failwith (sprintf "Unknown Type union discriminant %d" x)


let generate_one_field_accessors ~nodes_table ~scope ~mode ~discr_ofs field =
  let api_module = api_of_mode mode in
  let field_name = GenCommon.underscore_name (PS.Field.name_get field) in
  let discr_str =
    let discriminant_value = PS.Field.discriminant_value_get field in
    if discriminant_value = PS.Field.no_discriminant then
      ""
    else
      sprintf "~discr:{BA_.Discr.value=%u; BA_.Discr.byte_ofs=%u} "
        discriminant_value (discr_ofs * 2)
  in
  let (getters, setters) =
    let open PS.Field in
    begin match PS.Field.get field with
    | PS.Field.Group group ->
        let getters = [ "let " ^ field_name ^ "_get x = x" ] in
        (getters, [])
    | PS.Field.Slot slot ->
        let field_ofs = Uint32.to_int (PS.Field.Slot.offset_get slot) in
        let tp = PS.Field.Slot.type_get slot in
        let default = PS.Field.Slot.default_value_get slot in
        begin match (PS.Type.get tp, PS.Value.get default) with
        | (PS.Type.Void, PS.Value.Void) ->
            let getters =
              [ "let " ^ field_name ^ "_get x = ()" ]
            in
            let setters = [
              "let " ^ field_name ^ "_set x =";
              sprintf "  BA_.get_data_field %sx ~f:BA_.set_void" discr_str;
            ] in
            (getters, setters)
        | (PS.Type.Bool, PS.Value.Bool a) ->
            let getters = [
              "let " ^ field_name ^ "_get x =";
              sprintf "  %s.get_data_field x ~f:(%s.get_bit ~default:%s ~byte_ofs:%u \
                       ~bit_ofs:%u)"
                api_module
                api_module
                (if a then "true" else "false")
                (field_ofs / 8)
                (field_ofs mod 8);
            ] in
            let setters = [
              "let " ^ field_name ^ "_set x v =";
              sprintf "  BA_.get_data_field %sx ~f:(BA_.set_bit \
                        ~default:%s ~byte_ofs:%u ~bit_ofs:%u v)"
                discr_str
                (if a then "true" else "false")
                (field_ofs / 8)
                (field_ofs mod 8);
            ] in
            (getters, setters)
        | (PS.Type.Int8, PS.Value.Int8 a) ->
            let getters = [
              "let " ^ field_name ^ "_get x =";
              sprintf "  %s.get_data_field x ~f:(%s.get_int8 \
                       ~default:%d ~byte_ofs:%u)"
                api_module
                api_module
                a
                field_ofs;
            ] in
            let setters = [
              "let " ^ field_name ^ "_set_exn x v =";
              sprintf "  BA_.get_data_field %sx ~f:(BA_.set_int8 \
                       ~default:%u ~byte_ofs:%u v)"
                discr_str
                a
                field_ofs;
            ] in
            (getters, setters)
        | (PS.Type.Int16, PS.Value.Int16 a) ->
            let getters = [
              "let " ^ field_name ^ "_get x =";
              sprintf "  %s.get_data_field x ~f:(%s.get_int16 ~default:%d ~byte_ofs:%u)"
                api_module
                api_module
                a
                (field_ofs * 2)
            ] in
            let setters = [
              "let " ^ field_name ^ "_set_exn x v =";
              sprintf "  BA_.get_data_field %sx ~f:(BA_.set_int16 \
                       ~default:%u ~byte_ofs:%u v)"
                discr_str
                a
                (field_ofs * 2);
            ] in
            (getters, setters)
        | (PS.Type.Int32, PS.Value.Int32 a) ->
            let getters = [
              "let " ^ field_name ^ "_get x =";
              sprintf "  %s.get_data_field x ~f:(%s.get_int32 ~default:%sl ~byte_ofs:%u)"
                api_module
                api_module
                (Int32.to_string a)
                (field_ofs * 4);
              "let " ^ field_name ^ "_get_int_exn x =";
              "  Int32.to_int (" ^ field_name ^ "_get x)";
            ] in
            let setters = [
              "let " ^ field_name ^ "_set x v =";
              sprintf "  BA_.get_data_field %sx ~f:(BA_.set_int32 \
                       ~default:%sl ~byte_ofs:%u v)"
                discr_str
                (Int32.to_string a)
                (field_ofs * 4);
              "let " ^ field_name ^ "_set_int_exn x v = " ^
                field_name ^ "_set x (Int32.of_int v)";
            ] in
            (getters, setters)
        | (PS.Type.Int64, PS.Value.Int64 a) ->
            let getters = [
              "let " ^ field_name ^ "_get x =";
              sprintf "  %s.get_data_field x ~f:(%s.get_int64 ~default:%sL ~byte_ofs:%u)"
                api_module
                api_module
                (Int64.to_string a)
                (field_ofs * 8);
              "let " ^ field_name ^ "_get_int_exn x =";
              "  Int64.to_int (" ^ field_name ^ "_get x)";
            ] in
            let setters = [
              "let " ^ field_name ^ "_set x v =";
              sprintf "  BA_.get_data_field %sx ~f:(BA_.set_int64 \
                       ~default:%sL ~byte_ofs:%u v)"
                discr_str
                (Int64.to_string a)
                (field_ofs * 8);
              "let " ^ field_name ^ "_set_int_exn x v = " ^
                field_name ^ "_set x (Int64.of_int v)";
            ] in
            (getters, setters)
        | (PS.Type.Uint8, PS.Value.Uint8 a) ->
            let getters = [
              "let " ^ field_name ^ "_get x =";
              sprintf "  %s.get_data_field x ~f:(%s.get_uint8 \
                       ~default:%d ~byte_ofs:%u)"
                api_module
                api_module
                a
                field_ofs;
            ] in
            let setters = [
              "let " ^ field_name ^ "_set_exn x v =";
              sprintf "  BA_.get_data_field %sx ~f:(BA_.set_uint8 \
                       ~default:%u ~byte_ofs:%u v)"
                discr_str
                a
                field_ofs;
            ] in
            (getters, setters)
        | (PS.Type.Uint16, PS.Value.Uint16 a) ->
            let getters = [
              "let " ^ field_name ^ "_get x =";
              sprintf "  %s.get_data_field x ~f:(%s.get_uint16 \
                       ~default:%d ~byte_ofs:%u)"
                api_module
                api_module
                a
                (field_ofs * 2);
            ] in
            let setters = [
              "let " ^ field_name ^ "_set_exn x v =";
              sprintf "  BA_.get_data_field %sx ~f:(BA_.set_uint16 \
                       ~default:%u ~byte_ofs:%u v)"
                discr_str
                a
                (field_ofs * 2);
            ] in
            (getters, setters)
        | (PS.Type.Uint32, PS.Value.Uint32 a) ->
            let default =
              if Uint32.compare a Uint32.zero = 0 then
                "Uint32.zero"
              else
                sprintf "(Uint32.of_string \"%s\")" (Uint32.to_string a)
            in
            let getters = [
              "let " ^ field_name ^ "_get x =";
              sprintf "  %s.get_data_field x ~f:(%s.get_uint32 ~default:%s ~byte_ofs:%u)"
                api_module
                api_module
                default
                (field_ofs * 4);
              "let " ^ field_name ^ "_get_int_exn x =";
              "  Uint32.to_int (" ^ field_name ^ "_get x)";
            ] in 
            let setters = [
              "let " ^ field_name ^ "_set x v =";
              sprintf "  BA_.get_data_field %sx ~f:(BA_.set_uint32 \
                       ~default:%s ~byte_ofs:%u v)"
                discr_str
                default
                (field_ofs * 4);
              "let " ^ field_name ^ "_set_int_exn x v = " ^
                field_name ^ "_set x (Uint32.of_int v)";
            ] in
            (getters, setters)
        | (PS.Type.Uint64, PS.Value.Uint64 a) ->
            let default =
              if Uint64.compare a Uint64.zero = 0 then
                "Uint64.zero"
              else
                sprintf "(Uint64.of_string \"%s\")" (Uint64.to_string a)
            in
            let getters = [
              "let " ^ field_name ^ "_get x =";
              sprintf "  %s.get_data_field x ~f:(%s.get_uint64 \
                       ~default:%s ~byte_ofs:%u)"
                api_module
                api_module
                default
                (field_ofs * 8);
              "let " ^ field_name ^ "_get_int_exn x =";
              "  Uint64.to_int (" ^ field_name ^ "_get x)";
            ] in
            let setters = [
              "let " ^ field_name ^ "_set x v =";
              sprintf "  BA_.get_data_field %sx ~f:(BA_.set_uint64 \
                       ~default:%s ~byte_ofs:%u v)"
                discr_str
                default
                (field_ofs * 8);
              "let " ^ field_name ^ "_set_int_exn x v = " ^
                field_name ^ "_set x (Uint64.of_int v)";
            ] in
            (getters, setters)
        | (PS.Type.Float32, PS.Value.Float32 a) ->
            let default = Int32.to_string (Int32.bits_of_float a) in
            let getters = [
              "let " ^ field_name ^ "_get x =";
              sprintf "  %s.get_data_field x ~f:(%s.get_float32 \
                       ~default_bits:%sl ~byte_ofs:%u)"
                api_module
                api_module
                default
                (field_ofs * 4);
            ] in
            let setters = [
              "let " ^ field_name ^ "_set x v =";
              sprintf "  BA_.get_data_field %sx ~f:(BA_.set_float32 \
                       ~default_bits:%sl ~byte_ofs:%u v)"
                discr_str
                default
                (field_ofs * 4);
            ] in
            (getters, setters)
        | (PS.Type.Float64, PS.Value.Float64 a) ->
            let default = Int64.to_string (Int64.bits_of_float a) in
            let getters = [
              "let " ^ field_name ^ "_get x =";
              sprintf "  %s.get_data_field x ~f:(%s.get_float64 \
                       ~default_bits:%sL ~byte_ofs:%u)"
                api_module
                api_module
                default
                (field_ofs * 8);
            ] in
            let setters = [
              "let " ^ field_name ^ "_set x v =";
              sprintf "BA_.get_data_field %sx ~f:(BA_.set_float64 \
                       ~default_bits:%sL ~byte_ofs:%u v)"
                discr_str
                default
                (field_ofs * 8);
            ] in
            (getters, setters)
        | (PS.Type.Text, PS.Value.Text a) ->
            let getters = [
              "let " ^ field_name ^ "_get x =";
              sprintf "  %s.get_pointer_field x %u ~f:(%s.get_text ~default:\"%s\")"
                api_module
                field_ofs
                api_module
                (String.escaped a);
            ] in
            let setters = [
              "let " ^ field_name ^ "_set x v =";
              sprintf "  BA_.get_pointer_field %sx %u ~f:(BA_.set_text v)"
                discr_str
                field_ofs
            ] in
            (getters, setters)
        | (PS.Type.Data, PS.Value.Data a) ->
            let getters = [
              "let " ^ field_name ^ "_get x =";
              sprintf "  %s.get_pointer_field x %u ~f:(%s.get_blob ~default:\"%s\")"
                api_module
                field_ofs
                api_module
                (String.escaped a);
            ] in
            let setters = [
              "let " ^ field_name ^ "_set x v =";
              sprintf "  BA_.get_pointer_field %sx %u ~f:(BA_.set_blob v)"
                discr_str
                field_ofs
            ] in
            (getters, setters)
        | (PS.Type.List list_def, PS.Value.List pointer_slice_opt) ->
            let has_trivial_default =
              begin match pointer_slice_opt with
              | Some pointer_slice ->
                  begin match ReaderApi.decode_pointer pointer_slice with
                  | Capnp.Runtime.Pointer.Null -> true
                  | _ -> false
                  end
              | None ->
                  true
              end
            in
            if has_trivial_default then
              let list_type = PS.Type.List.element_type_get list_def in
              let getters = generate_list_getter ~nodes_table ~scope ~list_type
                ~mode ~field_name ~field_ofs
              in
              let setters = generate_list_setters ~nodes_table ~scope ~list_type
                ~discr_str ~field_name ~field_ofs
              in
              (getters, setters)
            else
              failwith "Default values for lists are not implemented."
        | (PS.Type.Enum enum_def, PS.Value.Enum val_uint16) ->
            let enum_id = PS.Type.Enum.type_id_get enum_def in
            let enum_node = Hashtbl.find_exn nodes_table enum_id in
            let getters = generate_enum_getter ~nodes_table ~scope ~enum_node
                ~mode ~field_name ~field_ofs ~default:val_uint16
            in
            let setters =
              (generate_enum_safe_setter ~nodes_table ~scope ~enum_node
                ~field_name ~field_ofs ~default:val_uint16 ~discr_str) @
              (generate_enum_unsafe_setter ~nodes_table ~scope ~enum_node
                 ~field_name ~field_ofs ~default:val_uint16 ~discr_str)
            in
            (getters, setters)
        | (PS.Type.Struct struct_def, PS.Value.Struct pointer_slice_opt) ->
            let has_trivial_default =
              begin match pointer_slice_opt with
              | Some pointer_slice ->
                  begin match ReaderApi.decode_pointer pointer_slice with
                  | Capnp.Runtime.Pointer.Null -> true
                  | _ -> false
                  end
              | None ->
                  true
              end
            in
            if has_trivial_default then
              let data_words, pointer_words =
                let id = PS.Type.Struct.type_id_get struct_def in
                let node = Hashtbl.find_exn nodes_table id in
                match PS.Node.get node with
                | PS.Node.Struct struct_def ->
                    (PS.Node.Struct.data_word_count_get struct_def,
                     PS.Node.Struct.pointer_count_get struct_def)
                | _ ->
                    failwith
                      "Decoded non-struct node where struct node was expected."
              in
              let getters =
                match mode with
                | Mode.Reader -> [
                    "let " ^ field_name ^ "_get x =";
                    sprintf "  RA_.get_pointer_field x %u ~f:RA_.get_struct"
                      field_ofs;
                  ]
                | Mode.Builder -> [
                    "let " ^ field_name ^ "_get x =";
                    sprintf "  BA_.get_pointer_field x %u \
                             ~f:(BA_.get_struct ~data_words:%u ~pointer_words:%u)"
                      field_ofs
                      data_words
                      pointer_words;
                  ]
              in
              let setters = [
                "let " ^ field_name ^ "_set_reader x v =";
                sprintf "  BA_.get_pointer_field %sx %u \
                         ~f:(BA_.set_struct ~data_words:%u ~pointer_words:%u v)"
                  discr_str
                  field_ofs
                  data_words
                  pointer_words;
                "let " ^ field_name ^ "_set_builder x v =";
                sprintf "  BA_.get_pointer_field %sx %u \
                         ~f:(BA_.set_struct ~data_words:%u ~pointer_words:%u (Some v))"
                  discr_str
                  field_ofs
                  data_words
                  pointer_words;
                "let " ^ field_name ^ "_init x =";
                sprintf "  BA_.get_pointer_field %sx %u \
                         ~f:(BA_.init_struct ~data_words:%u ~pointer_words:%u)"
                  discr_str
                  field_ofs
                  data_words
                  pointer_words;
              ] in
              (getters, setters)
            else
              failwith "Default values for structs are not implemented."
        | (PS.Type.Interface iface_def, PS.Value.Interface) ->
            let getters = [
              "let " ^ field_name ^
                "_get x = failwith \"not implemented (iface 2)\"";
            ] in
            let setters = [
              "let " ^ field_name ^
                "_set x v = failwith \"not implemented (iface 2)\"";
            ] in
            (getters, setters)
        | (PS.Type.AnyPointer, PS.Value.AnyPointer pointer) ->
            let getters = [
              "let " ^ field_name ^ "_get x =";
              sprintf "  %s.get_pointer_field x %u ~f:(fun x -> x)"
                api_module
                field_ofs;
            ] in
            let setters = [
              "let " ^ field_name ^
                "_set x v = failwith \"not implemented\"";
            ] in
            (getters, setters)
        | (PS.Type.Undefined x, _) ->
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
    | PS.Field.Undefined x ->
        failwith (sprintf "Unknown Field union discriminant %u." x)
    end
  in
  begin match mode with
  | Mode.Reader  -> getters
  | Mode.Builder -> getters @ setters
  end


(* Generate a function for unpacking a capnp union type as an OCaml variant. *)
let generate_union_getter ~nodes_table ~scope ~mode struct_def fields =
  match fields with
  | [] ->
      (* If there are no union fields, then suppress the union type *)
      []
  | _ ->
      let api_module = api_of_mode mode in
      let cases = List.fold_left fields ~init:[] ~f:(fun acc field ->
        let field_name = PS.Field.name_get field in
        let us_field_name = GenCommon.underscore_name field_name in
        let ctor_name = String.capitalize field_name in
        let field_value = PS.Field.discriminant_value_get field in
        let field_has_void_type =
          match PS.Field.get field with
          | PS.Field.Slot slot ->
              begin match PS.Type.get (PS.Field.Slot.type_get slot) with
              | PS.Type.Void -> true
              | _ -> false
              end
          | _ -> false
        in
        if field_has_void_type then
          (sprintf "  | %u -> %s"
            field_value
            ctor_name) :: acc
        else
          (sprintf "  | %u -> %s (%s_get x)"
            field_value
            ctor_name
            us_field_name) :: acc)
      in
      let header = [
        "let get x =";
        sprintf "  match %s.get_data_field x \
                 ~f:(%s.get_uint16 ~default:0 ~byte_ofs:%u) with"
          api_module
          api_module
          ((PS.Node.Struct.discriminant_offset_get_int_exn struct_def) * 2);
      ]
      in
      let undefined_name = GenCommon.mangle_field_undefined fields in
      let footer = [ sprintf "  | v -> %s v" undefined_name ] in
      (GenCommon.generate_union_type ~mode nodes_table scope fields) @
        header @ cases @ footer


(* Generate accessors for getting and setting a list of fields of a struct,
 * regardless of whether or not the fields are packed into a union.  (Getters
 * for fields packed inside a union are not exposed in the module signature.) *)
let generate_accessors ~nodes_table ~scope ~mode struct_def fields =
  let discr_ofs = PS.Node.Struct.discriminant_offset_get_int_exn struct_def in
  List.fold_left fields ~init:[] ~f:(fun acc field ->
    let x = generate_one_field_accessors ~nodes_table ~scope ~mode
        ~discr_ofs field
    in
    x @ acc)


(* Generate the OCaml module corresponding to a struct definition.  [scope] is a
 * stack of scope IDs corresponding to this lexical context, and is used to figure
 * out what module prefixes are required to properly qualify a type.
 *
 * Raises: Failure if the children of this node contain a cycle. *)
let rec generate_struct_node ~nodes_table ~scope ~nested_modules ~mode
    ~node struct_def =
  let unsorted_fields =
    C.Array.to_list (PS.Node.Struct.fields_get struct_def)
  in
  (* Sorting in reverse code order allows us to avoid a List.rev *)
  let all_fields = List.sort unsorted_fields ~cmp:(fun x y ->
    - (Int.compare (PS.Field.code_order_get x) (PS.Field.code_order_get y)))
  in
  let union_fields, non_union_fields = List.partition_tf all_fields
      ~f:(fun field ->
        (PS.Field.discriminant_value_get field) <> PS.Field.no_discriminant)
  in
  let union_accessors =
    (* Emit accessor functions first, because they are required for the
       variant-based code emitted by [generate_union_getter]. *)
    (generate_accessors ~nodes_table ~scope ~mode struct_def union_fields) @
      (generate_union_getter ~nodes_table ~scope ~mode struct_def union_fields)
  in
  let non_union_accessors =
    generate_accessors ~nodes_table ~scope ~mode struct_def non_union_fields
  in
  let unique_reader =
    GenCommon.make_unique_typename ~mode:Mode.Reader ~scope_mode:mode
      ~nodes_table node
  in
  let unique_builder =
    GenCommon.make_unique_typename ~mode:Mode.Builder ~scope_mode:mode
      ~nodes_table node
  in
  let header =
    match mode with
    | Mode.Reader -> [
        "type t = ro RA_.StructStorage.t option";
        "type builder_t = rw RA_.StructStorage.t";
        "type " ^ unique_reader ^ " = t";
        "type " ^ unique_builder ^ " = builder_t";
      ]
    | Mode.Builder -> [
        "type t = Reader." ^
          (GenCommon.get_fully_qualified_name nodes_table node) ^ ".builder_t";
        "type reader_t = Reader." ^
          (GenCommon.get_fully_qualified_name nodes_table node) ^ ".t";
        "type " ^ unique_builder ^ " = t";
        "type " ^ unique_reader ^ " = reader_t";
      ]
  in
  let footer =
    match mode with
    | Mode.Reader -> [
        "let of_message x = RA_.get_root_struct (RA_.Message.readonly x)";
      ]
    | Mode.Builder -> 
        let data_words    = PS.Node.Struct.data_word_count_get struct_def in
        let pointer_words = PS.Node.Struct.pointer_count_get struct_def in [
          sprintf "  let of_message x = BA_.get_root_struct \
                   ~data_words:%u ~pointer_words:%u x"
            data_words pointer_words;
          "let to_message x = x.BA_.StructStorage.data.MessageWrapper.Slice.msg";
          "let init_root ?message_size () =";
          sprintf "  BA_.alloc_root_struct ?message_size \
                   ~data_words:%u ~pointer_words:%u ()"
            data_words pointer_words;
        ]
  in
  header @
    nested_modules @
    union_accessors @
    non_union_accessors @
    footer


(* Generate the OCaml module and type signature corresponding to a node.  [scope] is
 * a stack of scope IDs corresponding to this lexical context, and is used to figure out
 * what module prefixes are required to properly qualify a type.
 *
 * Raises: Failure if the children of this node contain a cycle. *)
and generate_node
    ~(suppress_module_wrapper : bool)
    ~(nodes_table : (Uint64.t, PS.Node.t) Hashtbl.t)
    ~(scope : Uint64.t list)
    ~(mode : Mode.t)
    ~(node_name : string)
    (node : PS.Node.t)
: string list =
  let node_id = PS.Node.id_get node in
  let generate_nested_modules () =
    match Topsort.topological_sort nodes_table
            (GenCommon.children_of nodes_table node) with
    | Some child_nodes ->
        List.concat_map child_nodes ~f:(fun child ->
          let child_name = GenCommon.get_unqualified_name ~parent:node ~child in
          let child_node_id = PS.Node.id_get child in
          generate_node ~suppress_module_wrapper:false ~nodes_table
            ~scope:(child_node_id :: scope) ~mode ~node_name:child_name child)
    | None ->
        let error_msg = sprintf
          "The children of node %s (%s) have a cyclic dependency."
          (Uint64.to_string node_id)
          (PS.Node.display_name_get node)
        in
        failwith error_msg
  in
  match PS.Node.get node with
  | PS.Node.File ->
      generate_nested_modules ()
  | PS.Node.Struct struct_def ->
      let nested_modules = generate_nested_modules () in
      let body =
        generate_struct_node ~nodes_table ~scope ~nested_modules ~mode
          ~node struct_def
      in
      if suppress_module_wrapper then
        body
      else
        [ "module " ^ node_name ^ " = struct" ] @
          (apply_indent ~indent:"  " body) @
          [ "end" ]
  | PS.Node.Enum enum_def ->
      let nested_modules = generate_nested_modules () in
      let body =
        GenCommon.generate_enum_sig ~nodes_table ~scope ~nested_modules
          ~mode ~node enum_def
      in
      if suppress_module_wrapper then
        body
      else
        [ "module " ^ node_name ^ " = struct" ] @
          (apply_indent ~indent:"  " body) @
          [ "end" ]
  | PS.Node.Interface iface_def ->
      generate_nested_modules ()
  | PS.Node.Const const_def -> [
        "let " ^ (GenCommon.underscore_name node_name) ^ " = " ^
          (GenCommon.generate_constant ~nodes_table ~scope const_def);
      ]
  | PS.Node.Annotation annot_def ->
      generate_nested_modules ()
  | PS.Node.Undefined x ->
      failwith (sprintf "Unknown Node union discriminant %u" x)

