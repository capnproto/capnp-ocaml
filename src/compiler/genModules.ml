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

(* Workaround for missing Caml.Bytes in Core 112.35.00 *)
module CamlBytes = Bytes

open Core_kernel.Std

module PS        = GenCommon.PS
module Context   = GenCommon.Context
module Mode      = GenCommon.Mode
module C         = Capnp

module ReaderApi = struct
  open Capnp.Runtime
  module MessageWrapper = GenCommon.M
  include ReaderInc.Make(MessageWrapper)
end

let sprintf = Printf.sprintf
let apply_indent = GenCommon.apply_indent


let api_of_mode mode =
  match mode with
  | Mode.Reader  -> "RA_"
  | Mode.Builder -> "BA_"


(* Generate an accessor for decoding an enum type. *)
let generate_enum_getter ~context ~enum_node ~mode
    ~field_name ~field_ofs ~default =
  let api_module = api_of_mode mode in
  let unique_module_name =
    GenCommon.make_unique_enum_module_name ~context enum_node
  in [
    "let " ^ field_name ^ "_get x =";
    sprintf "  let discr = %s.get_uint16 ~default:%u x %u in"
      api_module
      default
      (field_ofs * 2);
    "  " ^ unique_module_name ^ ".decode discr";
  ]


(* Generate an accessor for setting the value of an enum. *)
let generate_enum_safe_setter ~context ~enum_node ~field_name
    ~field_ofs ~default ~discr_str =
  let unique_module_name =
    GenCommon.make_unique_enum_module_name ~context enum_node
  in [
    "let " ^ field_name ^ "_set x e =";
    sprintf
      "  BA_.set_uint16 %s~default:%u x %u (%s.encode_safe e)"
      discr_str
      default
      (field_ofs * 2)
      unique_module_name;
  ]


(* Generate an accessor for setting the value of an enum, permitting values
   which are not defined in the schema. *)
let generate_enum_unsafe_setter ~context ~enum_node ~field_name
    ~field_ofs ~default ~discr_str =
  let unique_module_name =
    GenCommon.make_unique_enum_module_name ~context enum_node
  in [
    "let " ^ field_name ^ "_set_unsafe x e =";
    sprintf
      "  BA_.set_uint16 %s~default:%u x %u (%s.encode_unsafe e)"
      discr_str
      default
      (field_ofs * 2)
      unique_module_name;
  ]


(* There is no get_enum() or get_enum_list() in the runtime API,
   because the enum values are schema-dependent.  This function
   will generate something appropriate for localized use. *)
let generate_enum_runtime_getters ~context ~mode enum_node =
  let api_module = api_of_mode mode in
  let unique_module_name =
    GenCommon.make_unique_enum_module_name ~context enum_node
  in
  let get_enum_list =
    match mode with
    | Mode.Reader -> [
        "let get_enum_list ?default struct_opt pointer_word =";
        "  RA_.get_list ?default (RA_.ListDecoders.Bytes2 (fun slice ->";
        "    " ^ unique_module_name ^ ".decode (Slice.get_uint16 slice 0)))";
        "    struct_opt pointer_word";
        "in";
      ]
    | Mode.Builder -> [
        "let get_enum_list ?default struct_storage pointer_word =";
        "  let codecs = BA_.ListCodecs.Bytes2 (";
        "    (fun slice -> " ^ unique_module_name ^
          ".decode (Slice.get_uint16 slice 0)),";
        "    (fun v slice -> Slice.set_uint16 slice 0 (" ^ unique_module_name ^
          ".encode_safe v)))";
        "  in";
        "  BA_.get_list ~storage_type:BA_.ListStorage.Bytes2 ~codecs";
        "    struct_storage pointer_word";
        "in";
      ]
  in [
    "let get_enum ~byte_ofs data_opt =";
    "  " ^ unique_module_name ^ ".decode (" ^ api_module ^
      ".get_uint16 ~default:0 ~byte_ofs data_opt)";
    "in";
  ] @ get_enum_list


(* There is no set_enum() or set_enum_list() in the runtime API,
   because the enum values are schema-dependent.  This function
   will generate something appropriate for localized use. *)
let generate_enum_runtime_setters ~context enum_node =
  let unique_module_name =
    GenCommon.make_unique_enum_module_name ~context enum_node
  in [
    "let set_enum ?discr struct_storage byte_ofs value =";
    "  BA_.set_uint16 ?discr ~default:0 struct_storage byte_ofs ("
    ^ unique_module_name ^ ".encode_safe value)";
    "in";
    "let set_enum_list ?discr struct_storage pointer_word value =";
    "  let codecs = BA_.ListCodecs.Bytes2 (";
    "    (fun slice -> " ^ unique_module_name ^
      ".decode (Slice.get_uint16 slice 0)),";
    "    (fun v slice -> Slice.set_uint16 slice 0 (" ^ unique_module_name ^
      ".encode_safe v)))";
    "  in";
    "  BA_.set_list ?discr ~storage_type:BA_.ListStorage.Bytes2 ~codecs";
    "    struct_storage pointer_word value";
    "in";
  ]


(* Generate a set of decoder functions for reading elements from a Cap'n Proto
   List<T>.  The resulting decoders could be passed as an argument to
   [Reader.get_list] in order to generate a getter for a list field. *)
let rec generate_list_element_decoder ~context ~scope list_def =
  let make_terminal_decoder element_name = [
      "let decoders = RA_.ListDecoders.Pointer (fun slice ->";
      (* Not super efficient, but this shouldn't be a hot path very often... *)
      "  let struct_storage = RA_.pointers_struct slice in";
      "  RA_.get_" ^ element_name ^ "_list (Some struct_storage) 0)";
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
          (generate_list_element_decoder ~context ~scope inner_list_def)
      in [
        "let decoders = RA_.ListDecoders.Pointer (fun slice ->";
      ] @ inner_decoder_decl @ [
        (* Not super efficient, but this shouldn't be a hot path very often... *)
        "  let struct_storage = RA_.pointers_struct slice in";
        "  RA_.get_list decoders (Some struct_storage) 0)";
        "in";
      ]
  | Enum enum_def ->
      let enum_id = PS.Type.Enum.type_id_get enum_def in
      let enum_node = Context.node context enum_id in
      let enum_getters =
        apply_indent ~indent:"  "
          (generate_enum_runtime_getters ~context ~mode:Mode.Reader enum_node)
      in [
        "let decoders =";
      ] @ enum_getters @ [
        "  RA_.ListDecoders.Pointer (fun slice ->";
        (* Not super efficient, but this shouldn't be a hot path very often... *)
        "    let struct_storage = RA_.pointers_struct slice in";
        "    get_enum_list (Some struct_storage) 0)";
        "in";
      ]
  | Interface _ ->
      failwith "not implemented"
  | AnyPointer _ ->
      failwith "not implemented"
  | Undefined x ->
       failwith (sprintf "Unknown Type union discriminant %d" x)


(* Generate a set of decoder/encoder functions for reading and writing
   elements from/to a Cap'n Proto List<T>.  The resulting codecs could
   be passed as an argument to [Builder.get_list] in order to generate
   a getter for a list field. *)
let rec generate_list_element_codecs ~context ~scope list_def =
  let make_terminal_codecs element_name = [
      "let codecs =";
      "  let decode slice =";
      "    let struct_storage = BA_.pointers_struct slice in";
      "    BA_.get_" ^ element_name ^ "_list struct_storage 0";
      "  in";
      "  let encode v slice =";
      "    let struct_storage = BA_.pointers_struct slice in";
      "    let _ = BA_.set_" ^ element_name ^ "_list struct_storage 0 v in ()";
      "  in";
      "  BA_.NC.ListCodecs.Pointer (decode, encode)";
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
  | Struct struct_def ->
      let data_words, pointer_words =
        let id = PS.Type.Struct.type_id_get struct_def in
        let node = Context.node context id in
        match PS.Node.get node with
        | PS.Node.Struct struct_def ->
            (PS.Node.Struct.data_word_count_get struct_def,
             PS.Node.Struct.pointer_count_get struct_def)
        | _ ->
            failwith
              "Decoded non-struct node where struct node was expected."
      in [
        "let codecs =";
        "  let decode slice =";
        "    let struct_storage = BA_.pointers_struct slice in";
        sprintf "    BA_.get_struct_list \
                 ~data_words:%u ~pointer_words:%u struct_storage 0"
          data_words pointer_words;
        "  in";
        "  let encode v slice =";
        "    let struct_storage = BA_.pointers_struct slice in";
        sprintf "    let _ = BA_.set_struct_list \
                 ~data_words:%u ~pointer_words:%u struct_storage 0 v in ()"
          data_words pointer_words;
        "  in";
        "  BA_.NC.ListCodecs.Pointer (decode, encode)";
        "in";
      ]
  | List inner_list_def ->
      let inner_codecs_decl =
        apply_indent ~indent:"  "
          (generate_list_element_codecs ~context ~scope inner_list_def)
      in [
        "let codecs ="; ] @ inner_codecs_decl @ [
        "  let decode slice =";
        "    let struct_storage = BA_.pointers_struct slice in";
        "    BA_.get_list ~codecs struct_storage 0";
        "  in";
        "  let encode v slice =";
        "    let struct_storage = BA_.pointers_struct slice in";
        "    BA_.set_list ~codecs struct_storage 0 v";
        "  in";
        "  BA_.NC.ListCodecs.Pointer (decode, encode)";
        "in";
      ]
  | Enum enum_def ->
      let enum_id = PS.Type.Enum.type_id_get enum_def in
      let enum_node = Context.node context enum_id in
      let enum_getters =
        apply_indent ~indent:"  "
          (generate_enum_runtime_getters ~context ~mode:Mode.Builder enum_node)
      in
      let enum_setters =
        apply_indent ~indent:"  "
          (generate_enum_runtime_setters ~context enum_node)
      in [
        "let codecs ="; ] @ enum_getters @ enum_setters @ [
        "  RA_.ListDecoders.Pointer (get_enum_list, set_enum_list)";
        "in";
      ]
  | Interface _ ->
      failwith "not implemented"
  | AnyPointer _ ->
      failwith "not implemented"
  | Undefined x ->
       failwith (sprintf "Unknown Type union discriminant %d" x)


(* Generate an accessor for retrieving a list of the given type. *)
let generate_list_getters ~context ~scope ~list_type ~mode
    ~field_name ~field_ofs ~default_str =
  let api_module = api_of_mode mode in
  let make_primitive_accessor element_name = [
      "let " ^ field_name ^ "_get x =";
      sprintf
        "  %s.get_%s_list%s x %u"
        api_module
        element_name
        default_str
        field_ofs;
    ]
  in
  let basic_getter =
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
            "let " ^ field_name ^ "_get x = ";
            sprintf "  RA_.get_struct_list%s x %u" default_str field_ofs;
          ]
        | Mode.Builder ->
            let data_words, pointer_words =
              let id = PS.Type.Struct.type_id_get struct_def in
              let node = Context.node context id in
              match PS.Node.get node with
              | PS.Node.Struct struct_def ->
                  (PS.Node.Struct.data_word_count_get struct_def,
                   PS.Node.Struct.pointer_count_get struct_def)
              | _ ->
                  failwith
                    "Decoded non-struct node where struct node was expected."
            in [
              "let " ^ field_name ^ "_get x = ";
              sprintf "  BA_.get_struct_list%s ~data_words:%u \
                       ~pointer_words:%u x %u"
                default_str
                data_words
                pointer_words
                field_ofs;
            ]
        end
    | List list_def ->
        begin match mode with
        | Mode.Reader ->
            let decoder_declaration =
              apply_indent ~indent:"  "
                (generate_list_element_decoder ~context ~scope list_def)
            in [
              "let " ^ field_name ^ "_get x =";
            ] @ decoder_declaration @ [
              sprintf "  RA_.get_list%s decoders x %u"
                default_str field_ofs;
            ]
        | Mode.Builder ->
            let codecs_declaration =
              apply_indent ~indent:"  "
                (generate_list_element_codecs ~context ~scope list_def)
            in [
              "let " ^ field_name ^ "_get x =";
            ] @ codecs_declaration @ [
              sprintf "  BA_.get_list%s \
                       ~storage_type:Capnp.Runtime.ListStorageType.Pointer ~codecs \
                       x %u"
                default_str field_ofs;
            ]
        end
    | Enum enum_def ->
        let enum_id = Enum.type_id_get enum_def in
        let enum_node = Context.node context enum_id in
        let unique_module_name =
          GenCommon.make_unique_enum_module_name ~context enum_node
        in
        begin match mode with
        | Mode.Reader -> [
            "let " ^ field_name ^ "_get x =";
            "  let slice_decoder slice =";
            "    " ^ unique_module_name ^
              ".decode (MessageWrapper.Slice.get_uint16 slice 0)";
            "  in";
            sprintf "  RA_.get_list%s (RA_.ListDecoders.Bytes2 slice_decoder) x %u"
              default_str field_ofs;
          ]
        | Mode.Builder -> [
            "let " ^ field_name ^ "_get x =";
            "  let slice_decoder slice =";
            "    " ^ unique_module_name ^
              ".decode (MessageWrapper.Slice.get_uint16 slice 0)";
            "  in";
            "  let slice_encoder v slice =";
            "    MessageWrapper.Slice.set_uint16 slice 0 (" ^ unique_module_name ^
              ".encode_safe v)";
            "  in";
            sprintf "  BA_.get_list%s" default_str;
            "    ~storage_type:Capnp.Runtime.ListStorageType.Bytes2";
            "    ~codecs:(BA_.NC.ListCodecs.Bytes2 (slice_decoder, slice_encoder))";
            sprintf "    x %u" field_ofs;
          ]
        end
    | Interface _ -> [
        "let " ^ field_name ^
          "_get x = failwith \"not implemented (type iface)\"";
        ]
    | AnyPointer _ -> [
        "let " ^ field_name ^
          "_get x = failwith \"not implemented (type anyptr)\"";
        ]
    | Undefined x ->
         failwith (sprintf "Unknown Type union discriminant %d" x)
  in
  basic_getter @ [
    "let " ^ field_name ^ "_get_list x =";
    "  Capnp.Array.to_list (" ^ field_name ^ "_get x)";
    "let " ^ field_name ^ "_get_array x =";
    "  Capnp.Array.to_array (" ^ field_name ^ "_get x)";
  ]


(* Generate accessors for setting or initializing a list of the given type. *)
let generate_list_setters ~context ~scope ~list_type
    ~discr_str ~field_name ~field_ofs =
  let make_primitive_setters element_name = [
    "let " ^ field_name ^ "_set x v =";
    sprintf "  BA_.set_%s_list %sx %u v" element_name discr_str field_ofs;
    "let " ^ field_name ^ "_init x n =";
    sprintf "  BA_.init_%s_list %sx %u n" element_name discr_str field_ofs;
  ] in
  let basic_setters =
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
          let node = Context.node context id in
          match PS.Node.get node with
          | PS.Node.Struct struct_def ->
              (PS.Node.Struct.data_word_count_get struct_def,
               PS.Node.Struct.pointer_count_get struct_def)
          | _ ->
              failwith
                "Decoded non-struct node where struct node was expected."
        in [
          "let " ^ field_name ^ "_set x v =";
          sprintf "  BA_.set_struct_list ~data_words:%u \
                   ~pointer_words:%u %sx %u v"
            data_words
            pointer_words
            discr_str
            field_ofs;
          "let " ^ field_name ^ "_init x n =";
          sprintf "  BA_.init_struct_list ~data_words:%u \
                   ~pointer_words:%u %sx %u n"
            data_words
            pointer_words
            discr_str
            field_ofs;
        ]
    | List list_def ->
        let codecs_declaration =
          apply_indent ~indent:"  "
            (generate_list_element_codecs ~context ~scope list_def)
        in [
          "let " ^ field_name ^ "_set x v =";
        ] @ codecs_declaration @ [
          "  BA_.set_list ~storage_type:Capnp.Runtime.ListStorageType.Pointer";
          sprintf "    ~codecs %sx %u v" discr_str field_ofs;
          "let " ^ field_name ^ "_init x n =";
        ] @ codecs_declaration @ [
          "  BA_.init_list ~storage_type:Capnp.Runtime.ListStorageType.Pointer";
          sprintf "    ~codecs %sx %u n" discr_str field_ofs;
        ]
    | Enum enum_def ->
        let enum_id = Enum.type_id_get enum_def in
        let enum_node = Context.node context enum_id in
        let unique_module_name =
          GenCommon.make_unique_enum_module_name ~context enum_node
        in
        let codecs = [
          "  let slice_decoder slice =";
          "    " ^ unique_module_name ^
            ".decode (MessageWrapper.Slice.get_uint16 slice 0)";
          "  in";
          "  let slice_encoder v slice =";
          "    MessageWrapper.Slice.set_uint16 slice 0 (" ^ unique_module_name ^
            ".encode_safe v)";
          "  in";
        ] in [
          "let " ^ field_name ^ "_set x v =";
        ] @ codecs @ [
          "  BA_.set_list ~storage_type:Capnp.Runtime.ListStorageType.Bytes2";
          "    ~codecs:(BA_.NC.ListCodecs.Bytes2 (slice_decoder, slice_encoder))";
          sprintf "    %sx %u v" discr_str field_ofs;
          "let " ^ field_name ^ "_init x n =";
        ] @ codecs @ [
          "  BA_.init_list ~storage_type:Capnp.Runtime.ListStorageType.Bytes2";
          "    ~codecs:(BA_.NC.ListCodecs.Bytes2 (slice_decoder, slice_encoder))";
          sprintf "    %sx %u n" discr_str field_ofs;
        ]
    | Interface _ -> [
        "let " ^ field_name ^
          "_set x v = failwith \"not implemented (type iface)\"";
        "let " ^ field_name ^
          "_init x n = failwith \"not implemented (type iface)\"";
        ]
    | AnyPointer _ -> [
        "let " ^ field_name ^
          "_set x v = failwith \"not implemented (type anyptr)\"";
        "let " ^ field_name ^
          "_init x n = failwith \"not implemented (type anyptr)\"";
        ]
    | Undefined x ->
         failwith (sprintf "Unknown Type union discriminant %d" x)
  in
  basic_setters @ [
    "let " ^ field_name ^ "_set_list x v =";
    "  let builder = " ^ field_name ^ "_init x (List.length v) in";
    "  let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in";
    "  builder";
    "let " ^ field_name ^ "_set_array x v =";
    "  let builder = " ^ field_name ^ "_init x (Array.length v) in";
    "  let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in";
    "  builder";
  ]


(* When generating the _init function for a group, it we need to
   (recursively) locate all fields belonging to the group and zero 
   them out one-by-one.  There's no better solution in general, as
   it's possible for the fields of multiple groups to be interleaved
   in memory.

   Note that this implementation does not attempt to detect overlapping
   fields... we just blindly set all the fields to binary zero.

   The resulting lines are returned in reverse order. *)
let rec generate_clear_group_fields_rev ~acc ~context ~group_id =
  let node = Context.node context group_id in
  match PS.Node.get node with
  | PS.Node.Struct struct_def ->
      let () = assert (PS.Node.Struct.is_group_get struct_def) in
      let acc =
        if PS.Node.Struct.discriminant_count_get struct_def <> 0 then
          (* Clear out the discriminant field *)
          [ sprintf "let () = BA_.set_int16 ~default:0 x %u 0 in"
              ((PS.Node.Struct.discriminant_offset_get_int_exn struct_def) * 2) ] @
            acc
        else
          acc
      in
      let fields = PS.Node.Struct.fields_get struct_def in
      C.Array.fold fields ~init:acc ~f:(fun lines field ->
        match PS.Field.get field with
        | PS.Field.Group group ->
            let group_id = PS.Field.Group.type_id_get group in
            generate_clear_group_fields_rev ~acc:lines ~context ~group_id
        | PS.Field.Slot slot ->
            let field_ofs = PS.Field.Slot.offset_get_int_exn slot in
            let tp = PS.Field.Slot.type_get slot in
            let clear_field =
              match PS.Type.get tp with
              | PS.Type.Void ->
                  []
              | PS.Type.Bool -> [
                  sprintf "let () = BA_.set_bit ~default:false x ~byte_ofs:%u \
                           ~bit_ofs:%u false in"
                    (field_ofs / 8) (field_ofs mod 8)
                ]
              | PS.Type.Int8
              | PS.Type.Uint8 -> [
                  sprintf "let () = BA_.set_int8 ~default:0 x %u 0 in"
                    field_ofs
                ]
              | PS.Type.Int16
              | PS.Type.Uint16
              | PS.Type.Enum _ -> [
                  sprintf "let () = BA_.set_int16 ~default:0 x %u 0 in"
                    (field_ofs * 2)
                ]
              | PS.Type.Int32
              | PS.Type.Uint32
              | PS.Type.Float32 -> [
                  sprintf "let () = BA_.set_int32 ~default:0l x %u 0l in"
                    (field_ofs * 4)
                ]
              | PS.Type.Int64
              | PS.Type.Uint64
              | PS.Type.Float64 -> [
                  sprintf "let () = BA_.set_int64 ~default:0L x %u 0L in"
                    (field_ofs * 8)
                ]
              | PS.Type.Text
              | PS.Type.Data
              | PS.Type.List _
              | PS.Type.Struct _
              | PS.Type.Interface _
              | PS.Type.AnyPointer _ -> [
                  "let () =";
                  "  let ptr = {";
                  "    pointers with";
                  "    MessageWrapper.Slice.start = \
                   pointers.MessageWrapper.Slice.start + " ^
                    (Int.to_string (field_ofs * 8)) ^ ";";
                  "    MessageWrapper.Slice.len = 8;";
                  "  } in";
                  "  let () = BA_.BOps.deep_zero_pointer ptr in";
                  "  MessageWrapper.Slice.set_int64 ptr 0 0L";
                  "in";
                ]
              | PS.Type.Undefined x ->
                  failwith (sprintf "Unknown Type union discriminant %u" x)
            in
            List.rev_append clear_field lines
        | PS.Field.Undefined x ->
            failwith (sprintf "Unknown Field union discriminant %u" x))
  | PS.Node.File
  | PS.Node.Enum _
  | PS.Node.Interface _
  | PS.Node.Const _
  | PS.Node.Annotation _ ->
      failwith "Found non-struct node when searching for a group."
  | PS.Node.Undefined x ->
      failwith (sprintf "Unknown Node union discriminant %u" x)


(* Generate the accessors for a single field. *)
let generate_one_field_accessors ~context ~node_id ~scope
    ~mode ~discr_ofs field =
  let api_module = api_of_mode mode in
  let field_name = GenCommon.underscore_name (PS.Field.name_get field) in
  let discriminant_value = PS.Field.discriminant_value_get field in
  let discr_str =
    if discriminant_value = PS.Field.no_discriminant then
      ""
    else
      sprintf "~discr:{BA_.Discr.value=%u; BA_.Discr.byte_ofs=%u} "
        discriminant_value (discr_ofs * 2)
  in
  let (getters, setters) =
    begin match PS.Field.get field with
    | PS.Field.Group group ->
        let getters = [ sprintf "let %s_get x = %s.cast_struct x" field_name api_module ] in
        let setters =
          let clear_fields =
            apply_indent ~indent:"  "
              (List.rev
                (generate_clear_group_fields_rev ~acc:[] ~context
                  ~group_id:(PS.Field.Group.type_id_get group)))
          in
          let set_discriminant =
            if discriminant_value = PS.Field.no_discriminant then
              []
            else
              apply_indent ~indent:"  " [
                "let () = BA_.set_opt_discriminant data";
                sprintf "  (Some {BA_.Discr.value=%u; BA_.Discr.byte_ofs=%u})"
                  discriminant_value (discr_ofs * 2);
                "in"
              ]
          in [
            "let " ^ field_name ^ "_init x =";
            "  let data = x.BA_.NM.StructStorage.data in";
            "  let pointers = x.BA_.NM.StructStorage.pointers in";
            (* Suppress unused variable warnings *)
            "  let () = ignore data in";
            "  let () = ignore pointers in"; ] @
            set_discriminant @
            clear_fields @
            [ sprintf "  %s.cast_struct x" api_module ]
        in
        (getters, setters)
    | PS.Field.Slot slot ->
        let field_ofs = PS.Field.Slot.offset_get_int_exn slot in
        let tp = PS.Field.Slot.type_get slot in
        let default = PS.Field.Slot.default_value_get slot in
        begin match (PS.Type.get tp, PS.Value.get default) with
        | (PS.Type.Void, PS.Value.Void) ->
            let getters =
              [ "let " ^ field_name ^ "_get x = ()" ]
            in
            let setters = [
              "let " ^ field_name ^ "_set x =";
              "  BA_.set_void " ^ discr_str ^ "x";
            ] in
            (getters, setters)
        | (PS.Type.Bool, PS.Value.Bool a) ->
            let getters = [
              "let " ^ field_name ^ "_get x =";
              sprintf "  %s.get_bit ~default:%s x ~byte_ofs:%u ~bit_ofs:%u"
                api_module
                (if a then "true" else "false")
                (field_ofs / 8)
                (field_ofs mod 8);
            ] in
            let setters = [
              "let " ^ field_name ^ "_set x v =";
              sprintf "  BA_.set_bit %s~default:%s x ~byte_ofs:%u ~bit_ofs:%u v"
                discr_str
                (if a then "true" else "false")
                (field_ofs / 8)
                (field_ofs mod 8);
            ] in
            (getters, setters)
        | (PS.Type.Int8, PS.Value.Int8 a) ->
            let getters = [
              "let " ^ field_name ^ "_get x =";
              sprintf "  %s.get_int8 ~default:(%d) x %u"
                api_module
                a
                field_ofs;
            ] in
            let setters = [
              "let " ^ field_name ^ "_set_exn x v =";
              sprintf "  BA_.set_int8 %s~default:(%d) x %u v"
                discr_str
                a
                field_ofs;
            ] in
            (getters, setters)
        | (PS.Type.Int16, PS.Value.Int16 a) ->
            let getters = [
              "let " ^ field_name ^ "_get x =";
              sprintf "  %s.get_int16 ~default:(%d) x %u"
                api_module
                a
                (field_ofs * 2);
            ] in
            let setters = [
              "let " ^ field_name ^ "_set_exn x v =";
              sprintf "  BA_.set_int16 %s~default:(%d) x %u v"
                discr_str
                a
                (field_ofs * 2);
            ] in
            (getters, setters)
        | (PS.Type.Int32, PS.Value.Int32 a) ->
            let getters = [
              "let " ^ field_name ^ "_get x =";
              sprintf "  %s.get_int32 ~default:(%sl) x %u"
                api_module
                (Int32.to_string a)
                (field_ofs * 4);
              "let " ^ field_name ^ "_get_int_exn x =";
              "  Capnp.Runtime.Util.int_of_int32_exn (" ^ field_name ^ "_get x)";
            ] in
            let setters = [
              "let " ^ field_name ^ "_set x v =";
              sprintf "  BA_.set_int32 %s~default:(%sl) x %u v"
                discr_str
                (Int32.to_string a)
                (field_ofs * 4);
              "let " ^ field_name ^ "_set_int_exn x v = " ^
                field_name ^ "_set x (Capnp.Runtime.Util.int32_of_int_exn v)";
            ] in
            (getters, setters)
        | (PS.Type.Int64, PS.Value.Int64 a) ->
            let getters = [
              "let " ^ field_name ^ "_get x =";
              sprintf "  %s.get_int64 ~default:(%sL) x %u"
                api_module
                (Int64.to_string a)
                (field_ofs * 8);
              "let " ^ field_name ^ "_get_int_exn x =";
              "  Capnp.Runtime.Util.int_of_int64_exn (" ^ field_name ^ "_get x)";
            ] in
            let setters = [
              "let " ^ field_name ^ "_set x v =";
              sprintf "  BA_.set_int64 %s~default:(%sL) x %u v"
                discr_str
                (Int64.to_string a)
                (field_ofs * 8);
              "let " ^ field_name ^ "_set_int x v = " ^
                field_name ^ "_set x (Int64.of_int v)";
            ] in
            (getters, setters)
        | (PS.Type.Uint8, PS.Value.Uint8 a) ->
            let getters = [
              "let " ^ field_name ^ "_get x =";
              sprintf "  %s.get_uint8 ~default:%u x %u"
                api_module
                a
                field_ofs;
            ] in
            let setters = [
              "let " ^ field_name ^ "_set_exn x v =";
              sprintf "  BA_.set_uint8 %s~default:%u x %u v"
                discr_str
                a
                field_ofs;
            ] in
            (getters, setters)
        | (PS.Type.Uint16, PS.Value.Uint16 a) ->
            let getters = [
              "let " ^ field_name ^ "_get x =";
              sprintf "  %s.get_uint16 ~default:%u x %u"
                api_module
                a
                (field_ofs * 2);
            ] in
            let setters = [
              "let " ^ field_name ^ "_set_exn x v =";
              sprintf "  BA_.set_uint16 %s~default:%u x %u v"
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
              sprintf "  %s.get_uint32 ~default:%s x %u"
                api_module
                default
                (field_ofs * 4);
              "let " ^ field_name ^ "_get_int_exn x =";
              "  Capnp.Runtime.Util.int_of_uint32_exn (" ^ field_name ^ "_get x)";
            ] in
            let setters = [
              "let " ^ field_name ^ "_set x v =";
              sprintf "  BA_.set_uint32 %s~default:%s x %u v"
                discr_str
                default
                (field_ofs * 4);
              "let " ^ field_name ^ "_set_int_exn x v = " ^
                field_name ^ "_set x (Capnp.Runtime.Util.uint32_of_int_exn v)";
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
              sprintf "  %s.get_uint64 ~default:%s x %u"
                api_module
                default
                (field_ofs * 8);
              "let " ^ field_name ^ "_get_int_exn x =";
              "  Capnp.Runtime.Util.int_of_uint64_exn (" ^ field_name ^ "_get x)";
            ] in
            let setters = [
              "let " ^ field_name ^ "_set x v =";
              sprintf "  BA_.set_uint64 %s~default:%s x %u v"
                discr_str
                default
                (field_ofs * 8);
              "let " ^ field_name ^ "_set_int_exn x v = " ^
                field_name ^ "_set x (Capnp.Runtime.Util.uint64_of_int_exn v)";
            ] in
            (getters, setters)
        | (PS.Type.Float32, PS.Value.Float32 a) ->
            let default = Int32.to_string (Int32.bits_of_float a) in
            let getters = [
              "let " ^ field_name ^ "_get x =";
              sprintf "  %s.get_float32 ~default_bits:(%sl) x %u"
                api_module
                default
                (field_ofs * 4);
            ] in
            let setters = [
              "let " ^ field_name ^ "_set x v =";
              sprintf "  BA_.set_float32 %s~default_bits:(%sl) x %u v"
                discr_str
                default
                (field_ofs * 4);
            ] in
            (getters, setters)
        | (PS.Type.Float64, PS.Value.Float64 a) ->
            let default = Int64.to_string (Int64.bits_of_float a) in
            let getters = [
              "let " ^ field_name ^ "_get x =";
              sprintf "  %s.get_float64 ~default_bits:(%sL) x %u"
                api_module
                default
                (field_ofs * 8);
            ] in
            let setters = [
              "let " ^ field_name ^ "_set x v =";
              sprintf "  BA_.set_float64 %s~default_bits:(%sL) x %u v"
                discr_str
                default
                (field_ofs * 8);
            ] in
            (getters, setters)
        | (PS.Type.Text, PS.Value.Text a) ->
            let getters = [
              "let has_" ^ field_name ^ " x =";
              sprintf "  %s.has_field x %u" api_module field_ofs;
              "let " ^ field_name ^ "_get x =";
              sprintf "  %s.get_text ~default:\"%s\" x %u"
                api_module (String.escaped a) field_ofs;
            ] in
            let setters = [
              "let " ^ field_name ^ "_set x v =";
              sprintf "  BA_.set_text %sx %u v"
                discr_str field_ofs;
            ] in
            (getters, setters)
        | (PS.Type.Data, PS.Value.Data a) ->
            let getters = [
              "let has_" ^ field_name ^ " x =";
              sprintf "  %s.has_field x %u" api_module field_ofs;
              "let " ^ field_name ^ "_get x =";
              sprintf "  %s.get_blob ~default:\"%s\" x %u"
                api_module (String.escaped a) field_ofs;
            ] in
            let setters = [
              "let " ^ field_name ^ "_set x v =";
              sprintf "  BA_.set_blob %sx %u v"
                discr_str field_ofs;
            ] in
            (getters, setters)
        | (PS.Type.List list_def, PS.Value.List pointer_slice_opt) ->
            let default_str =
              match pointer_slice_opt with
              | Some pointer_slice ->
                  begin match ReaderApi.deref_list_pointer pointer_slice with
                  | Some _default_storage ->
                      let ident = Defaults.make_ident node_id field_name in
                      begin match mode with
                      | Mode.Reader ->
                          " ~default:" ^ (Defaults.reader_string_of_ident ident)
                      | Mode.Builder ->
                          " ~default:" ^ (Defaults.builder_string_of_ident ident)
                      end
                  | None ->
                      ""
                  end
              | None ->
                  ""
            in
            let list_type = PS.Type.List.element_type_get list_def in
            let getters = [
              "let has_" ^ field_name ^ " x =";
              sprintf "  %s.has_field x %u" api_module field_ofs;
              ] @ (generate_list_getters ~context ~scope ~list_type
                ~mode ~field_name ~field_ofs ~default_str)
            in
            let setters = generate_list_setters ~context ~scope ~list_type
              ~discr_str ~field_name ~field_ofs
            in
            (getters, setters)
        | (PS.Type.Enum enum_def, PS.Value.Enum val_uint16) ->
            let enum_id = PS.Type.Enum.type_id_get enum_def in
            let enum_node = Context.node context enum_id in
            let getters = generate_enum_getter ~context ~enum_node
                ~mode ~field_name ~field_ofs ~default:val_uint16
            in
            let setters =
              (generate_enum_safe_setter ~context ~enum_node
                ~field_name ~field_ofs ~default:val_uint16 ~discr_str) @
              (generate_enum_unsafe_setter ~context ~enum_node
                 ~field_name ~field_ofs ~default:val_uint16 ~discr_str)
            in
            (getters, setters)
        | (PS.Type.Struct struct_def, PS.Value.Struct pointer_slice_opt) ->
            let reader_default_str, builder_default_str =
              match pointer_slice_opt with
              | Some pointer_slice ->
                  begin match ReaderApi.deref_struct_pointer pointer_slice with
                  | Some _ ->
                      let ident = Defaults.make_ident node_id field_name in
                      (" ~default:" ^ (Defaults.reader_string_of_ident ident),
                       " ~default:" ^ (Defaults.builder_string_of_ident ident))
                  | None ->
                      ("", "")
                  end
              | None ->
                  ("", "")
            in
            let data_words, pointer_words =
              let id = PS.Type.Struct.type_id_get struct_def in
              let node = Context.node context id in
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
                  "let has_" ^ field_name ^ " x =";
                  sprintf "  RA_.has_field x %u" field_ofs;
                  "let " ^ field_name ^ "_get x =";
                  sprintf "  RA_.get_struct%s x %u" reader_default_str field_ofs;
                  "let " ^ field_name ^ "_get_pipelined x =";
                  sprintf "  RPC.Untyped.struct_field x %u" field_ofs;
                ]
              | Mode.Builder -> [
                  "let has_" ^ field_name ^ " x =";
                  sprintf "  BA_.has_field x %u" field_ofs;
                  "let " ^ field_name ^ "_get x =";
                  sprintf "  BA_.get_struct%s ~data_words:%u ~pointer_words:%u x %u"
                    builder_default_str
                    data_words
                    pointer_words
                    field_ofs;
                ]
            in
            let setters = [
              "let " ^ field_name ^ "_set_reader x v =";
              sprintf "  BA_.set_struct ~data_words:%u ~pointer_words:%u %sx %u v"
                data_words
                pointer_words
                discr_str
                field_ofs;
              "let " ^ field_name ^ "_set_builder x v =";
              sprintf "  BA_.set_struct ~data_words:%u ~pointer_words:%u \
                       %sx %u (Some v)"
                data_words
                pointer_words
                discr_str
                field_ofs;
              "let " ^ field_name ^ "_init x =";
              sprintf "  BA_.init_struct ~data_words:%u ~pointer_words:%u %sx %u"
                data_words
                pointer_words
                discr_str
                field_ofs;
            ] in
            (getters, setters)
        | (PS.Type.Interface _, PS.Value.Interface) ->
            let getters = [
              "let " ^ field_name ^ "_get x =";
              sprintf "  %s.get_interface RPC.Untyped.get_cap x %u" api_module field_ofs;
              "let " ^ field_name ^ "_get_pipelined x = ";
              sprintf "  RPC.Untyped.capability_field x %u" field_ofs;
            ] in
            let setters = [
              "let " ^ field_name ^ "_set x v =";
              sprintf "  BA_.set_interface %sx %u v" discr_str field_ofs;
              "    ~add_attachment:RPC.Untyped.add_cap";
              "    ~clear_attachment:RPC.Untyped.clear_cap";
            ] in
            (getters, setters)
        | (PS.Type.AnyPointer _, PS.Value.AnyPointer pointer_slice_opt) ->
            let reader_default_str, builder_default_str =
              match pointer_slice_opt with
              | Some pointer_bytes ->
                  let pointer_val = GenCommon.M.Slice.get_int64 pointer_bytes 0 in
                  if Int64.compare pointer_val Int64.zero <> 0 then
                    let ident = Defaults.make_ident node_id field_name in
                    (" ~default:" ^ (Defaults.reader_string_of_ident ident),
                     " ~default:" ^ (Defaults.builder_string_of_ident ident))
                  else
                    ("", "")
              | None ->
                  ("", "")
            in
            let getters = [
              "let " ^ field_name ^ "_get x =";
              sprintf "  %s.get_pointer%s x %u"
                api_module
                (if mode = Mode.Reader then reader_default_str
                 else builder_default_str)
                field_ofs;
              "let " ^ field_name ^ "_get_interface x =";
              sprintf "  %s.get_interface RPC.Untyped.get_cap x %u"
                api_module
                field_ofs;
            ] in
            let setters = [
              "let " ^ field_name ^ "_set x v =";
              sprintf "  BA_.set_pointer %sx %u (Some v)"
                discr_str
                field_ofs;
              "let " ^ field_name ^ "_set_reader x v =";
              sprintf "  BA_.set_pointer %sx %u v"
                discr_str
                field_ofs;
              "let " ^ field_name ^ "_set_interface x v =";
              sprintf "  BA_.set_interface %sx %u v"
                discr_str
                field_ofs;
              "    ~add_attachment:RPC.Untyped.add_cap";
              "    ~clear_attachment:RPC.Untyped.clear_cap";
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
        | (PS.Type.AnyPointer _, _) ->
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
let generate_union_getter ~context ~scope ~mode struct_def fields =
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
        sprintf "  match %s.get_uint16 ~default:0 x %u with"
          api_module
          ((PS.Node.Struct.discriminant_offset_get_int_exn struct_def) * 2);
      ]
      in
      let undefined_name = GenCommon.mangle_field_undefined fields in
      let footer = [ sprintf "  | v -> %s v" undefined_name ] in
      (GenCommon.generate_union_type ~context ~mode scope fields) @
        header @ cases @ footer


(* Generate accessors for getting and setting a list of fields of a struct,
 * regardless of whether or not the fields are packed into a union.  (Getters
 * for fields packed inside a union are not exposed in the module signature.) *)
let generate_accessors ~context ~node ~scope ~mode struct_def fields =
  let discr_ofs = PS.Node.Struct.discriminant_offset_get_int_exn struct_def in
  let node_id = PS.Node.id_get node in
  List.fold_left fields ~init:[] ~f:(fun acc field ->
    let x = generate_one_field_accessors ~context ~node_id ~scope
        ~mode ~discr_ofs field in
    x @ acc)


(* FIXME: clean up redundant logic with [generate_list_element_decoder] *)
let generate_list_constant ~context ~scope ~node_id ~list_name list_def =
  let declare_decoders element_name = [
    "let decoders = RA_." ^ element_name ^ "_list_decoders in";
    ]
  in
  let decoders =
    let open PS.Type in
    let contained_type = List.element_type_get list_def in
    match get contained_type with
    | Void     -> declare_decoders "void"
    | Bool     -> declare_decoders "bit"
    | Int8     -> declare_decoders "int8"
    | Int16    -> declare_decoders "int16"
    | Int32    -> declare_decoders "int32"
    | Int64    -> declare_decoders "int64"
    | Uint8    -> declare_decoders "uint8"
    | Uint16   -> declare_decoders "uint16"
    | Uint32   -> declare_decoders "uint32"
    | Uint64   -> declare_decoders "uint64"
    | Float32  -> declare_decoders "float32"
    | Float64  -> declare_decoders "float64"
    | Text     -> declare_decoders "text"
    | Data     -> declare_decoders "blob"
    | Struct _ -> declare_decoders "struct"
    | List inner_list_def ->
        let inner_decoder_decl =
          apply_indent ~indent:"  "
            (generate_list_element_decoder ~context ~scope inner_list_def)
        in [
          "let decoders = RA_.ListDecoders.Pointer (fun slice ->";
        ] @ inner_decoder_decl @ [
          "  decoders)";
        ]
    | Enum enum_def ->
      let enum_id = PS.Type.Enum.type_id_get enum_def in
      let enum_node = Context.node context enum_id in
        let unique_module_name =
          GenCommon.make_unique_enum_module_name ~context enum_node
        in [
          "let decoders =";
          "  RA_.ListDecoders.Bytes2 (fun slice ->";
          "    " ^ unique_module_name ^
            ".decode (MessageWrapper.Slice.get_uint16 slice 0))";
          "in";
        ]
    | Interface _ ->
        failwith "not implemented"
    | AnyPointer _ ->
        failwith "not implemented"
    | Undefined x ->
        failwith (sprintf "Unknown Type union discriminant %d" x)
  in
  decoders @ [
    "RA_.make_array_readonly " ^
      (Defaults.reader_string_of_ident (Defaults.make_ident node_id list_name)) ^
      " decoders";
  ]


(* Generate a definition for a constant. *)
let generate_constant ~context ~scope ~node ~node_name const_def =
  let const_type = PS.Node.Const.type_get const_def in
  let const_val  = PS.Node.Const.value_get const_def in
  let open PS in
  match (Type.get const_type, Value.get const_val) with
  | (Type.Void, Value.Void) ->
      [ "()" ]
  | (Type.Bool, Value.Bool a) ->
      if a then [ "true" ] else [ "false" ]
  | (Type.Int8, Value.Int8 a)
  | (Type.Int16, Value.Int16 a)
  | (Type.Uint8, Value.Uint8 a)
  | (Type.Uint16, Value.Uint16 a) ->
      [ Int.to_string a ]
  | (Type.Int32, Value.Int32 a) ->
      [ (Int32.to_string a) ^ "l" ]
  | (Type.Int64, Value.Int64 a) ->
      [ (Int64.to_string a) ^ "L" ]
  | (Type.Uint32, Value.Uint32 a) ->
      [ sprintf "(Uint32.of_string \"%s\")" (Uint32.to_string a) ]
  | (Type.Uint64, Value.Uint64 a) ->
      [ sprintf "(Uint64.of_string \"%s\")" (Uint64.to_string a) ]
  | (Type.Float32, Value.Float32 a) ->
      [ sprintf "(Int32.float_of_bits (%sl))"
          (Int32.to_string (Int32.bits_of_float a)) ]
  | (Type.Float64, Value.Float64 a) ->
      [ sprintf "(Int64.float_of_bits (%sL))"
          (Int64.to_string (Int64.bits_of_float a)) ]
  | (Type.Text, Value.Text a)
  | (Type.Data, Value.Data a) ->
      [ "\"" ^ (String.escaped a) ^ "\"" ]
  | (Type.List list_def, Value.List _) ->
      let node_id = PS.Node.id_get node in
      let list_name = GenCommon.underscore_name node_name in
      generate_list_constant ~context ~scope ~node_id ~list_name list_def
  | (Type.Enum _, Value.Enum enum_val) ->
      let const_type = PS.Node.Const.type_get const_def in
      let enum_node =
        match PS.Type.get const_type with
        | PS.Type.Enum enum_def ->
            let enum_id = PS.Type.Enum.type_id_get enum_def in
            Context.node context enum_id
        | _ ->
            failwith "Decoded non-enum node where enum node was expected."
      in
      let enumerants =
        match PS.Node.get enum_node with
        | PS.Node.Enum enum_group -> PS.Node.Enum.enumerants_get enum_group
        | _ -> failwith "Decoded non-enum node where enum node was expected."
      in
      let undefined_name = GenCommon.mangle_enum_undefined enumerants in
      let scope_relative_name =
        GenCommon.get_scope_relative_name ~context scope enum_node in
      if enum_val >= C.Array.length enumerants then
        [ sprintf "%s.%s %u" scope_relative_name
            (String.capitalize undefined_name) enum_val ]
      else
        let enumerant = C.Array.get enumerants enum_val in
        [ sprintf "%s.%s"
            scope_relative_name
            (String.capitalize (PS.Enumerant.name_get enumerant)) ]
  | (Type.Struct _, Value.Struct _) ->
      let node_id = PS.Node.id_get node in
      let name = GenCommon.underscore_name node_name in
      [ "Some " ^
          (Defaults.reader_string_of_ident (Defaults.make_ident node_id name)) ]
  | (Type.Interface _, Value.Interface) ->
      failwith "Interface constants are not yet implemented."
  | (Type.AnyPointer _, Value.AnyPointer _) ->
      failwith "AnyPointer constants are not yet implemented."
  | (Type.Undefined x, _) ->
      failwith (sprintf "Unknown Value union discriminant %u." x)
  (* All other cases represent an ill-formed default value in the plugin request *)
  | (Type.Void, _)
  | (Type.Bool, _)
  | (Type.Int8, _)
  | (Type.Int16, _)
  | (Type.Int32, _)
  | (Type.Int64, _)
  | (Type.Uint8, _)
  | (Type.Uint16, _)
  | (Type.Uint32, _)
  | (Type.Uint64, _)
  | (Type.Float32, _)
  | (Type.Float64, _)
  | (Type.Text, _)
  | (Type.Data, _)
  | (Type.List _, _)
  | (Type.Enum _, _)
  | (Type.Struct _, _)
  | (Type.Interface _, _)
  | (Type.AnyPointer _, _) ->
      let err_msg = sprintf
          "The value provided for the constant with name \"%s\" has \
           an unexpected type."
          node_name
      in
      failwith err_msg


(* Generate the OCaml module corresponding to a struct definition.  [scope] is a
 * stack of scope IDs corresponding to this lexical context, and is used to figure
 * out what module prefixes are required to properly qualify a type. *)
let rec generate_struct_node ?uq_name ~context ~scope ~nested_modules ~mode
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
    (generate_accessors ~context ~node ~scope ~mode struct_def union_fields) @
      (generate_union_getter ~context ~scope ~mode struct_def union_fields)
  in
  let non_union_accessors =
    generate_accessors ~context ~node ~scope ~mode struct_def non_union_fields
  in
  let unique_struct = GenCommon.make_unique_typename ?uq_name ~context node in
  let header =
    match mode with
    | Mode.Reader -> [
        "type struct_t = " ^ unique_struct;
        "type t = struct_t reader_t";
      ]
    | Mode.Builder -> [
        "type struct_t = " ^ unique_struct;
        "type t = struct_t builder_t";
      ]
  in
  let footer =
    match mode with
    | Mode.Reader -> [
        "let of_message x = RA_.get_root_struct (RA_.Message.readonly x)";
        "let of_builder x = Some (RA_.StructStorage.readonly x)";
      ]
    | Mode.Builder ->
        let data_words    = PS.Node.Struct.data_word_count_get struct_def in
        let pointer_words = PS.Node.Struct.pointer_count_get struct_def in [
          sprintf "let of_message x = BA_.get_root_struct \
                   ~data_words:%u ~pointer_words:%u x"
            data_words pointer_words;
          "let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg";
          "let to_reader x = Some (RA_.StructStorage.readonly x)";
          "let init_root ?message_size () =";
          sprintf "  BA_.alloc_root_struct ?message_size \
                   ~data_words:%u ~pointer_words:%u ()"
            data_words pointer_words;
          "let init_pointer ptr =";
          sprintf "  BA_.init_struct_pointer ptr \
                   ~data_words:%u ~pointer_words:%u"
            data_words pointer_words;
        ]
  in
  header @
    nested_modules @
    union_accessors @
    non_union_accessors @
    footer


and generate_methods ~context ~scope ~nested_modules ~mode ~node_name interface_def : string list =
  (* todo: superclasses *)
  let module Method = GenCommon.Method in
  let methods = Method.methods_of_interface ~context interface_def in
  let make_auto m phase =
    match Method.auto_struct phase m with
    | `Existing _ -> []
    | `Auto (struct_node, struct_def, mod_name) ->
      let uq_name = Method.capnp_name m in
      let body = generate_struct_node
          ~uq_name ~context ~scope ~nested_modules:[] ~mode ~node:struct_node struct_def
      in
      [ "module " ^ mod_name ^ " = struct" ] @
      (apply_indent ~indent:"  " body) @
      [ "end" ]
  in
  let structs =
    List.map methods ~f:(fun m ->
        make_auto m Method.Params @
        make_auto m Method.Results
      )
    |> List.concat
  in
  match mode with
  | Mode.Reader ->
    let client =
      let body =
        List.map methods ~f:(fun m ->
            [
              sprintf "let %s_method : (t, %s, %s) RPC.Capability.method_t = RPC.Untyped.define_method ~interface_id ~method_id:%d"
                (Method.ocaml_name m)
                (Method.(payload_type Params) ~context ~scope ~mode m)
                (Method.(payload_type Results) ~context ~scope ~mode m)
                (Method.id m);
            ]
          )
        |> List.concat
      in
      let method_printers =
        List.map methods ~f:(fun m ->
            sprintf "| %d -> Some %S" (Method.id m) (Method.capnp_name m)
          )
      in
      body @
      [
        "let method_name = function";
      ] @ apply_indent ~indent:"  " method_printers @ [
        "  | _ -> None";
        sprintf "let () = Capnp.RPC.Registry.register ~interface_id ~name:%S method_name"
          node_name;
      ]
    in
    nested_modules @ structs @ client
  | Mode.Builder ->
    let service =
      let body =
        List.map methods ~f:(fun m ->
            sprintf "method virtual %s_impl : (%s, %s) RPC.Service.method_t"
              (Method.ocaml_name m)
              (Method.(payload_type Params) ~context ~scope ~mode m)
              (Method.(payload_type Results) ~context ~scope ~mode m)
          )
      in
      let dispatch_body =
        List.map methods ~f:(fun m ->
            sprintf "| %d -> RPC.Untyped.abstract_method self#%s_impl"
              (Method.id m)
              (Method.ocaml_name m)
          )
      in
      [ "class virtual service = object (self)";
        "  method release = ()";
        "  method dispatch ~interface_id:i ~method_id =";
        "    if i <> interface_id then RPC.Untyped.unknown_interface ~interface_id";
        "    else match method_id with";
      ] @ apply_indent ~indent:"    " dispatch_body @
      [ "    | x -> RPC.Untyped.unknown_method ~interface_id ~method_id";
        sprintf "  method pp f = Format.pp_print_string f %S" node_name;
      ] @
      (apply_indent ~indent:"  " body) @
      [ "end";
        "let local (service:#service) =";
        "  RPC.Untyped.local service";
      ]
    in
    nested_modules @ structs @ service


(* Generate the OCaml module and type signature corresponding to a node.  [scope] is
 * a stack of scope IDs corresponding to this lexical context, and is used to figure out
 * what module prefixes are required to properly qualify a type.
 *
 * Raises: Failure if the children of this node contain a cycle. *)
and generate_node
    ~(suppress_module_wrapper : bool)
    ~(context : Context.codegen_context_t)
    ~(scope : Uint64.t list)
    ~(mode : Mode.t)
    ~(node_name : string)
    (node : PS.Node.t)
: string list =
  let generate_nested_modules () =
    let child_nodes = GenCommon.children_of ~context node in
    List.concat_map child_nodes ~f:(fun child ->
        let child_name = GenCommon.get_unqualified_name ~parent:node ~child in
        let child_node_id = PS.Node.id_get child in
        generate_node ~suppress_module_wrapper:false ~context
          ~scope:(child_node_id :: scope) ~mode ~node_name:child_name child)
  in
  match PS.Node.get node with
  | PS.Node.File ->
      generate_nested_modules ()
  | PS.Node.Struct struct_def ->
      let nested_modules = generate_nested_modules () in
      let body =
        generate_struct_node ~context ~scope ~nested_modules ~mode
          ~node struct_def
      in
      if suppress_module_wrapper then
        body
      else
        [ "module " ^ node_name ^ " = struct" ] @
          (apply_indent ~indent:"  " body) @
          [ "end" ]
  | PS.Node.Enum enum_def ->
      let unique_module_name =
        GenCommon.make_unique_enum_module_name ~context node
      in
      let body =
        (generate_nested_modules ()) @
        (GenCommon.generate_enum_sig ~unique_module_name enum_def)
      in
      if suppress_module_wrapper then
        body
      else
        [ "module " ^ node_name ^ " = struct" ] @
          (apply_indent ~indent:"  " body) @
          [ "end" ]
  | PS.Node.Interface iface_def ->
      let nested_modules = generate_nested_modules () in
      let unique_reader = GenCommon.make_unique_typename ~context ~mode:Mode.Reader node in
      let body = [
        "type t = " ^ unique_reader;
        sprintf "let interface_id = Uint64.of_string \"%s\"" (Uint64.to_string_hex (PS.Node.id_get node));
      ] @ generate_methods ~context ~scope ~nested_modules ~mode ~node_name iface_def
      in
      if suppress_module_wrapper then
        body
      else
        [ "module " ^ node_name ^ " = struct" ] @
          (apply_indent ~indent:"  " body) @
          [ "end" ]
  | PS.Node.Const const_def -> [
      let typ =
        PS.Node.Const.type_get const_def
        |> GenCommon.type_name ~context ~mode:Mode.Reader ~scope_mode:mode scope
      in
      sprintf "let %s : %s =" (GenCommon.underscore_name node_name) typ
    ] @ (apply_indent ~indent:"  "
          (generate_constant ~context ~scope ~node ~node_name const_def))
  | PS.Node.Annotation _ ->
      generate_nested_modules ()
  | PS.Node.Undefined x ->
      failwith (sprintf "Unknown Node union discriminant %u" x)


(* Update the default-value context with default values associated with
   the specified struct. *)
let update_defaults_context_struct ~defaults_context ~node ~struct_def =
  let fields = PS.Node.Struct.fields_get struct_def in
  C.Array.iter fields ~f:(fun field ->
    let field_name = GenCommon.underscore_name (PS.Field.name_get field) in
    let node_id = PS.Node.id_get node in
    match PS.Field.get field with
    | PS.Field.Group _ ->
        ()
    | PS.Field.Slot slot ->
        let tp = PS.Field.Slot.type_get slot in
        let default = PS.Field.Slot.default_value_get slot in
        begin match (PS.Type.get tp, PS.Value.get default) with
        | (PS.Type.Struct _, PS.Value.Struct pointer_slice_opt) ->
            begin match pointer_slice_opt with
            | Some pointer_slice ->
                begin match ReaderApi.deref_struct_pointer pointer_slice with
                | Some default_storage ->
                    let ident = Defaults.make_ident node_id field_name in
                    Defaults.add_struct defaults_context ident default_storage
                | None ->
                    ()
                end
            | None ->
                ()
            end
        | (PS.Type.List _, PS.Value.List pointer_slice_opt) ->
            begin match pointer_slice_opt with
            | Some pointer_slice ->
                begin match ReaderApi.deref_list_pointer pointer_slice with
                | Some default_storage ->
                    let ident = Defaults.make_ident node_id field_name in
                    Defaults.add_list defaults_context ident default_storage
                | None ->
                    ()
                end
            | None ->
                ()
            end
        | (PS.Type.AnyPointer _, PS.Value.AnyPointer pointer_slice_opt) ->
            begin match pointer_slice_opt with
            | Some pointer_slice ->
                begin match ReaderApi.decode_pointer pointer_slice with
                | C.Runtime.Pointer.Null ->
                    ()
                | _ ->
                    let ident = Defaults.make_ident node_id field_name in
                    Defaults.add_pointer defaults_context ident pointer_slice
                end
            | None ->
                ()
            end
        | _ ->
            ()
        end
    | PS.Field.Undefined x ->
        failwith (sprintf "Unknown Field union discriminant %u." x))


(* Update the default-value context with default values associated with
   the specified constant. *)
let update_defaults_context_constant ~defaults_context
    ~node ~node_name ~const_def =
  let node_id = PS.Node.id_get node in
  let name = GenCommon.underscore_name node_name in
  let open PS.Value in
  let const_val = PS.Node.Const.value_get const_def in
  match get const_val with
  | Struct pointer_slice_opt ->
      begin match pointer_slice_opt with
      | Some pointer_slice ->
          begin match ReaderApi.deref_struct_pointer pointer_slice with
          | Some default_storage ->
              let ident = Defaults.make_ident node_id name in
              Defaults.add_struct defaults_context ident default_storage
          | None ->
              failwith (sprintf
                  "Struct constant \"%s\" has unexpected type."
                  name)
          end
      | None ->
          assert false
      end
  | List pointer_slice_opt ->
      begin match pointer_slice_opt with
      | Some pointer_slice ->
          begin match ReaderApi.deref_list_pointer pointer_slice with
          | Some default_storage ->
              let ident = Defaults.make_ident node_id name in
              Defaults.add_list defaults_context ident default_storage
          | None ->
              failwith (sprintf
                  "List constant \"%s\" has unexpected type."
                  name)
          end
      | None ->
          assert false
      end
  | AnyPointer pointer_slice_opt ->
      begin match pointer_slice_opt with
      | Some pointer_slice ->
          let ident = Defaults.make_ident node_id name in
          Defaults.add_pointer defaults_context ident pointer_slice
      | None ->
          assert false
      end
  | _ ->
      ()


(* Construct new context for managing struct and list default values,
   and fill the context with all the default values associated with the
   node (recursively). *)
let rec build_defaults_context
    ?(defaults_context : Defaults.t option)
    ~(context : Context.codegen_context_t)
    ~(node_name : string)
    (node : PS.Node.t)
  : Defaults.t =
  let ctx =
    match defaults_context with
    | Some x -> x
    | None -> Defaults.create ()
  in
  let child_nodes = GenCommon.children_of ~context node in
  let () = List.iter child_nodes ~f:(fun child_node ->
      let child_name = GenCommon.get_unqualified_name
          ~parent:node ~child:child_node
      in
      let _ = build_defaults_context ~defaults_context:ctx ~context
          ~node_name:child_name child_node in ())
  in
  match PS.Node.get node with
  | PS.Node.Struct struct_def ->
      let () = update_defaults_context_struct
          ~defaults_context:ctx ~node ~struct_def
      in
      ctx
  | PS.Node.Const const_def ->
      let () = update_defaults_context_constant
          ~defaults_context:ctx ~node ~node_name ~const_def
      in
      ctx
  | PS.Node.File
  | PS.Node.Enum _
  | PS.Node.Interface _
  | PS.Node.Annotation _ ->
      ctx
  | PS.Node.Undefined x ->
      failwith (sprintf "Unknown Node union discriminant %u" x)


