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

(* Runtime support which is common to both Reader and Builder interfaces. *)

open Core.Std


let sizeof_uint32 = 4
let sizeof_uint64 = 8

module Make (MessageWrapper : MessageSig.S) = struct
  let invalid_msg = Message.invalid_msg
  type ro = Message.ro
  type rw = Message.rw

  include MessageWrapper

  let bounds_check_slice_exn ?err (slice : 'cap Slice.t) : unit =
    let open Slice in
    if slice.segment_id < 0 ||
      slice.segment_id >= Message.num_segments slice.msg ||
      slice.start < 0 ||
      slice.start + slice.len > Segment.length (Slice.get_segment slice)
    then
      let error_msg =
        match err with
        | None -> "pointer referenced a memory region outside the message"
        | Some msg -> msg
      in
      invalid_msg error_msg
    else
      ()


  (** Get the range of bytes associated with a pointer stored in a struct. *)
  let ss_get_pointer
      (struct_storage : 'cap StructStorage.t)
      (word : int)           (* Struct-relative pointer index *)
    : 'cap Slice.t option =  (* Returns None if storage is too small for this word *)
    let pointers = struct_storage.StructStorage.pointers in
    let start = word * sizeof_uint64 in
    let len   = sizeof_uint64 in
    if start + len <= pointers.Slice.len then
      Some {
        pointers with
        Slice.start = pointers.Slice.start + start;
        Slice.len   = len
      }
    else
      None

  (* Given a range of eight bytes corresponding to a cap'n proto pointer,
     decode the information stored in the pointer. *)
  let decode_pointer (pointer_bytes : 'cap Slice.t) : Pointer.t =
    let pointer64 = Slice.get_int64 pointer_bytes 0 in
    if Int64.compare pointer64 Int64.zero = 0 then
      Pointer.Null
    else
      let module B = Pointer.Bitfield in
      let tag = Int64.bit_and pointer64 B.tag_mask in
      if Int64.compare tag B.tag_val_list = 0 then
        Pointer.List (ListPointer.decode pointer64)
      else if Int64.compare tag B.tag_val_struct = 0 then
        Pointer.Struct (StructPointer.decode pointer64)
      else if Int64.compare tag B.tag_val_far = 0 then
        Pointer.Far (FarPointer.decode pointer64)
      else if Int64.compare tag B.tag_val_other = 0 then
        Pointer.Other (OtherPointer.decode pointer64)
      else
        invalid_msg "pointer has undefined type tag"


  (* Given a list pointer descriptor, construct the corresponding list storage
     descriptor. *)
  let make_list_storage
      ~(message : 'cap Message.t)     (* Message of interest *)
      ~(segment_id : int)             (* Segment ID where list storage is found *)
      ~(segment_offset : int)         (* Segment offset where list storage is found *)
      ~(list_pointer : ListPointer.t)
    : 'cap ListStorage.t =
    let make_list_storage_aux ~num_words ~num_elements ~storage_type =
      let storage = {
        Slice.msg        = message;
        Slice.segment_id = segment_id;
        Slice.start      = segment_offset;
        Slice.len        = num_words * sizeof_uint64;
      } in
      let () = bounds_check_slice_exn
        ~err:"list pointer describes invalid storage region" storage
      in {
        ListStorage.storage      = storage;
        ListStorage.storage_type = storage_type;
        ListStorage.num_elements = num_elements;
      }
    in
    let open ListPointer in
    match list_pointer.element_type with
    | Void ->
        make_list_storage_aux ~num_words:0
          ~num_elements:list_pointer.num_elements ~storage_type:ListStorageType.Empty
    | OneBitValue ->
        make_list_storage_aux
          ~num_words:(Util.ceil_ratio list_pointer.num_elements 64)
          ~num_elements:list_pointer.num_elements ~storage_type:ListStorageType.Bit
    | OneByteValue ->
        make_list_storage_aux
          ~num_words:(Util.ceil_ratio list_pointer.num_elements 8)
          ~num_elements:list_pointer.num_elements
          ~storage_type:ListStorageType.Bytes1
    | TwoByteValue ->
        make_list_storage_aux
          ~num_words:(Util.ceil_ratio list_pointer.num_elements 4)
          ~num_elements:list_pointer.num_elements
          ~storage_type:ListStorageType.Bytes2
    | FourByteValue ->
        make_list_storage_aux
          ~num_words:(Util.ceil_ratio list_pointer.num_elements 2)
          ~num_elements:list_pointer.num_elements
          ~storage_type:ListStorageType.Bytes4
    | EightByteValue ->
        make_list_storage_aux ~num_words:list_pointer.num_elements
          ~num_elements:list_pointer.num_elements
          ~storage_type:ListStorageType.Bytes8
    | EightBytePointer ->
        make_list_storage_aux ~num_words:list_pointer.num_elements
          ~num_elements:list_pointer.num_elements
          ~storage_type:ListStorageType.Pointer
    | Composite ->
        let struct_tag_bytes = {
          Slice.msg        = message;
          Slice.segment_id = segment_id;
          Slice.start      = segment_offset;
          Slice.len        = sizeof_uint64;
        } in
        let () = bounds_check_slice_exn
            ~err:"composite list pointer describes invalid storage region"
            struct_tag_bytes
        in
        begin match decode_pointer struct_tag_bytes with
        | Pointer.Struct struct_pointer ->
            let module SP = StructPointer in
            let num_words = list_pointer.num_elements in
            let num_elements = struct_pointer.SP.offset in
            let words_per_element =
              struct_pointer.SP.data_words + struct_pointer.SP.pointer_words
            in
            if num_elements * words_per_element > num_words then
              invalid_msg "composite list pointer describes invalid word count"
            else
              make_list_storage_aux ~num_words ~num_elements
                ~storage_type:(ListStorageType.Composite
                   (struct_pointer.SP.data_words, struct_pointer.SP.pointer_words))
        | _ ->
            invalid_msg "composite list pointer has malformed element type tag"
        end


  (* Given a description of a cap'n proto far pointer, get the object which
     the pointer points to. *)
  let rec deref_far_pointer
      (far_pointer : FarPointer.t)
      (message : 'cap Message.t)
    : 'cap Object.t =
    let open FarPointer in
    match far_pointer.landing_pad with
    | NormalPointer ->
        let next_pointer_bytes = {
          Slice.msg        = message;
          Slice.segment_id = far_pointer.segment_id;
          Slice.start      = far_pointer.offset * sizeof_uint64;
          Slice.len        = sizeof_uint64;
        } in
        let () = bounds_check_slice_exn
          ~err:"far pointer describes invalid landing pad" next_pointer_bytes
        in
        deref_pointer next_pointer_bytes
    | TaggedFarPointer ->
        let content_pointer_bytes = {
          Slice.msg        = message;
          Slice.segment_id = far_pointer.segment_id;
          Slice.start      = far_pointer.offset * sizeof_uint64;
          Slice.len        = sizeof_uint64;
        } in
        let tag_bytes = {
          content_pointer_bytes with
          Slice.start = Slice.get_end content_pointer_bytes;
        } in
        match (decode_pointer content_pointer_bytes, decode_pointer tag_bytes) with
        | (Pointer.Far content_pointer, Pointer.List list_pointer) ->
            Object.List (make_list_storage
              ~message
              ~segment_id:content_pointer.FarPointer.segment_id
              ~segment_offset:(content_pointer.FarPointer.offset * sizeof_uint64)
              ~list_pointer)
        | (Pointer.Far content_pointer, Pointer.Struct struct_pointer) ->
            let data = {
              Slice.msg = message;
              Slice.segment_id = content_pointer.FarPointer.segment_id;
              Slice.start = content_pointer.FarPointer.offset * sizeof_uint64;
              Slice.len = struct_pointer.StructPointer.data_words * sizeof_uint64;
            } in
            let pointers = {
              data with
              Slice.start = Slice.get_end data;
              Slice.len =
                struct_pointer.StructPointer.pointer_words * sizeof_uint64;
            } in
            let () = bounds_check_slice_exn
                ~err:"struct-tagged far pointer describes invalid data region"
                data
            in
            let () = bounds_check_slice_exn
                ~err:"struct-tagged far pointer describes invalid pointers region"
                pointers
            in
            Object.Struct { StructStorage.data; StructStorage.pointers; }
        | _ ->
            invalid_msg "tagged far pointer points to invalid landing pad"


  (* Given a range of eight bytes which represent a pointer, get the object which
     the pointer points to. *)
  and deref_pointer (pointer_bytes : 'cap Slice.t) : 'cap Object.t =
    match decode_pointer pointer_bytes with
    | Pointer.Null ->
        Object.None
    | Pointer.List list_pointer ->
        Object.List (make_list_storage
          ~message:pointer_bytes.Slice.msg
          ~segment_id:pointer_bytes.Slice.segment_id
          ~segment_offset:((Slice.get_end pointer_bytes) +
                             (list_pointer.ListPointer.offset * sizeof_uint64))
          ~list_pointer)
    | Pointer.Struct struct_pointer ->
        let open StructPointer in
        let data = {
          pointer_bytes with
          Slice.start =
            (Slice.get_end pointer_bytes) + (struct_pointer.offset * sizeof_uint64);
          Slice.len = struct_pointer.data_words * sizeof_uint64;
        } in
        let pointers = {
          data with
          Slice.start = Slice.get_end data;
          Slice.len   = struct_pointer.pointer_words * sizeof_uint64;
        } in
        let () = bounds_check_slice_exn
          ~err:"struct pointer describes invalid data region" data
        in
        let () = bounds_check_slice_exn
          ~err:"struct pointer describes invalid pointers region" pointers
        in
        Object.Struct { StructStorage.data; StructStorage.pointers; }
    | Pointer.Far far_pointer ->
        deref_far_pointer far_pointer pointer_bytes.Slice.msg
    | Pointer.Other (OtherPointer.Capability index) ->
        Object.Capability index


  module ListDecoders = struct
    type ('cap, 'a) struct_decoders_t = {
      bytes     : 'cap Slice.t -> 'a;
      pointer   : 'cap Slice.t -> 'a;
      composite : 'cap StructStorage.t -> 'a;
    }

    type ('cap, 'a) t =
      | Empty of (unit -> 'a)
      | Bit of (bool -> 'a)
      | Bytes1 of ('cap Slice.t -> 'a)
      | Bytes2 of ('cap Slice.t -> 'a)
      | Bytes4 of ('cap Slice.t -> 'a)
      | Bytes8 of ('cap Slice.t -> 'a)
      | Pointer of ('cap Slice.t -> 'a)
      | Struct of ('cap, 'a) struct_decoders_t
  end


  module ListCodecs = struct
    type 'a struct_codecs_t = {
      bytes     : (rw Slice.t -> 'a) * ('a -> rw Slice.t -> unit);
      pointer   : (rw Slice.t -> 'a) * ('a -> rw Slice.t -> unit);
      composite : (rw StructStorage.t -> 'a) * ('a -> rw StructStorage.t -> unit);
    }

    type 'a t =
      | Empty of (unit -> 'a) * ('a -> unit)
      | Bit of (bool -> 'a) * ('a -> bool)
      | Bytes1 of (rw Slice.t -> 'a) * ('a -> rw Slice.t -> unit)
      | Bytes2 of (rw Slice.t -> 'a) * ('a -> rw Slice.t -> unit)
      | Bytes4 of (rw Slice.t -> 'a) * ('a -> rw Slice.t -> unit)
      | Bytes8 of (rw Slice.t -> 'a) * ('a -> rw Slice.t -> unit)
      | Pointer of (rw Slice.t -> 'a) * ('a -> rw Slice.t -> unit)
      | Struct of 'a struct_codecs_t
  end


  let make_array_readonly
      (list_storage : 'cap ListStorage.t)
      (decoders : ('cap, 'a) ListDecoders.t)
    : (ro, 'a, 'cap ListStorage.t) InnerArray.t =
    let make_element_slice ls i byte_count = {
      ls.ListStorage.storage with
      Slice.start = ls.ListStorage.storage.Slice.start + (i * byte_count);
      Slice.len = byte_count;
    } in
    let length = list_storage.ListStorage.num_elements in
    let get_unsafe =
      match list_storage.ListStorage.storage_type with
      | ListStorageType.Empty ->
          begin match decoders with
          | ListDecoders.Empty decode ->
              fun ls i -> decode ()
          | _ ->
              invalid_msg
                "decoded List<Void> where a different list type was expected"
          end
      | ListStorageType.Bit ->
          begin match decoders with
          | ListDecoders.Bit decode ->
              fun ls i ->
                let byte_ofs = i / 8 in
                let bit_ofs  = i mod 8 in
                let byte_val =
                  Slice.get_uint8 ls.ListStorage.storage byte_ofs
                in
                decode ((byte_val land (1 lsl bit_ofs)) <> 0)
          | _ ->
              invalid_msg
                "decoded List<Bool> where a different list type was expected"
          end
      | ListStorageType.Bytes1 ->
          begin match decoders with
          | ListDecoders.Bytes1 decode
          | ListDecoders.Struct { ListDecoders.bytes = decode; _ } ->
              fun ls i -> decode (make_element_slice ls i 1)
          | _ ->
              invalid_msg
                "decoded List<1 byte> where a different list type was expected"
          end
      | ListStorageType.Bytes2 ->
          begin match decoders with
          | ListDecoders.Bytes2 decode
          | ListDecoders.Struct { ListDecoders.bytes = decode; _ } ->
              fun ls i -> decode (make_element_slice ls i 2)
          | _ ->
              invalid_msg
                "decoded List<2 byte> where a different list type was expected"
          end
      | ListStorageType.Bytes4 ->
          begin match decoders with
          | ListDecoders.Bytes4 decode
          | ListDecoders.Struct { ListDecoders.bytes = decode; _ } ->
              fun ls i -> decode (make_element_slice ls i 4)
          | _ ->
              invalid_msg
                "decoded List<4 byte> where a different list type was expected"
          end
      | ListStorageType.Bytes8 ->
          begin match decoders with
          | ListDecoders.Bytes8 decode
          | ListDecoders.Struct { ListDecoders.bytes = decode; _ } ->
              fun ls i -> decode (make_element_slice ls i 8)
          | _ ->
              invalid_msg
                "decoded List<8 byte> where a different list type was expected"
          end
      | ListStorageType.Pointer ->
          begin match decoders with
          | ListDecoders.Pointer decode
          | ListDecoders.Struct { ListDecoders.pointer = decode; _ } ->
              fun ls i ->
                decode (make_element_slice ls i sizeof_uint64)
          | _ ->
              invalid_msg
                "decoded List<pointer> a different list type was expected"
          end
      | ListStorageType.Composite (data_words, pointer_words) ->
          let data_size     = data_words * sizeof_uint64 in
          let pointers_size = pointer_words * sizeof_uint64 in
          let total_size    = data_size + pointers_size in
          let make_storage ls i =
            (* Skip over the composite tag word *)
            let content_offset =
              ls.ListStorage.storage.Slice.start + sizeof_uint64
            in
            let data = {
              ls.ListStorage.storage with
              Slice.start = content_offset + (i * total_size);
              Slice.len = data_size;
            } in
            let pointers = {
              data with
              Slice.start = Slice.get_end data;
              Slice.len   = pointers_size;
            } in
            { StructStorage.data; StructStorage.pointers; }
          in
          let make_bytes_handler ~size ~decode =
            if data_words = 0 then
              invalid_msg
                "decoded List<composite> with empty data region where data was expected"
            else
              (fun ls i ->
                let struct_storage = make_storage ls i in
                let slice = {
                  struct_storage.StructStorage.data with
                  Slice.len = size
                } in
                decode slice)
          in
          begin match decoders with
          | ListDecoders.Empty decode ->
              fun ls i -> decode ()
          | ListDecoders.Bit decode ->
              if data_words = 0 then
                invalid_msg
                  "decoded List<composite> with empty data region where data was expected"
              else
                fun ls i ->
                  let struct_storage = make_storage ls i in
                  let first_byte = Slice.get_uint8 struct_storage.StructStorage.data 0 in
                  let is_set = (first_byte land 0x1) <> 0 in
                  decode is_set
          | ListDecoders.Bytes1 decode ->
              make_bytes_handler ~size:1 ~decode
          | ListDecoders.Bytes2 decode ->
              make_bytes_handler ~size:2 ~decode
          | ListDecoders.Bytes4 decode ->
              make_bytes_handler ~size:4 ~decode
          | ListDecoders.Bytes8 decode ->
              make_bytes_handler ~size:8 ~decode
          | ListDecoders.Pointer decode ->
              if pointer_words = 0 then
                invalid_msg
                  "decoded List<composite> with empty pointers region where \
                   pointers were expected"
              else
                (fun ls i ->
                  let struct_storage = make_storage ls i in
                  let slice = {
                    struct_storage.StructStorage.pointers with
                    Slice.len = sizeof_uint64
                  } in
                  decode slice)
          | ListDecoders.Struct struct_decoders ->
              fun ls i ->
                let struct_storage = make_storage ls i in
                struct_decoders.ListDecoders.composite struct_storage
          end
    in {
      InnerArray.length;
      InnerArray.init = InnerArray.invalid_init;
      InnerArray.get_unsafe;
      InnerArray.set_unsafe = InnerArray.invalid_set_unsafe;
      InnerArray.storage = Some list_storage;
    }


  let make_array_readwrite
      ~(list_storage : rw ListStorage.t)
      ~(init : int -> rw ListStorage.t)
      ~(codecs : 'a ListCodecs.t)
    : (rw, 'a, rw ListStorage.t) InnerArray.t =
    let make_element_slice ls i byte_count = {
      ls.ListStorage.storage with
      Slice.start = ls.ListStorage.storage.Slice.start + (i * byte_count);
      Slice.len = byte_count;
    } in
    let length = list_storage.ListStorage.num_elements in
    let get_unsafe, set_unsafe =
      match list_storage.ListStorage.storage_type with
      | ListStorageType.Empty ->
          begin match codecs with
          | ListCodecs.Empty (decode, encode) ->
              let get ls i = decode () in
              let set ls i v = encode v in
              (get, set)
          | _ ->
              invalid_msg
                "decoded List<Void> where a different list type was expected"
          end
      | ListStorageType.Bit ->
          begin match codecs with
          | ListCodecs.Bit (decode, encode) ->
              let get ls i =
                let byte_ofs = i / 8 in
                let bit_ofs  = i mod 8 in
                let byte_val =
                  Slice.get_uint8 ls.ListStorage.storage byte_ofs
                in
                decode ((byte_val land (1 lsl bit_ofs)) <> 0)
              in
              let set ls i v =
                let byte_ofs = i / 8 in
                let bit_ofs  = i mod 8 in
                let bitmask  = 1 lsl bit_ofs in
                let old_byte_val =
                  Slice.get_uint8 ls.ListStorage.storage byte_ofs
                in
                let new_byte_val =
                  if encode v then
                    old_byte_val lor bitmask
                  else
                    old_byte_val land (lnot bitmask)
                in
                Slice.set_uint8 ls.ListStorage.storage byte_ofs new_byte_val
              in
              (get, set)
          | _ ->
              invalid_msg
                "decoded List<Bool> where a different list type was expected"
          end
      | ListStorageType.Bytes1 ->
          begin match codecs with
          | ListCodecs.Bytes1 (decode, encode)
          | ListCodecs.Struct { ListCodecs.bytes = (decode, encode); _ } ->
              let get ls i = decode (make_element_slice ls i 1) in
              let set ls i v = encode v (make_element_slice ls i 1) in
              (get, set)
          | _ ->
              invalid_msg
                "decoded List<1 byte> where a different list type was expected"
          end
      | ListStorageType.Bytes2 ->
          begin match codecs with
          | ListCodecs.Bytes2 (decode, encode)
          | ListCodecs.Struct { ListCodecs.bytes = (decode, encode); _ } ->
              let get ls i = decode (make_element_slice ls i 2) in
              let set ls i v = encode v (make_element_slice ls i 2) in
              (get, set)
          | _ ->
              invalid_msg
                "decoded List<2 byte> where a different list type was expected"
          end
      | ListStorageType.Bytes4 ->
          begin match codecs with
          | ListCodecs.Bytes4 (decode, encode)
          | ListCodecs.Struct { ListCodecs.bytes = (decode, encode); _ } ->
              let get ls i = decode (make_element_slice ls i 4) in
              let set ls i v = encode v (make_element_slice ls i 4) in
              (get, set)
          | _ ->
              invalid_msg
                "decoded List<4 byte> where a different list type was expected"
          end
      | ListStorageType.Bytes8 ->
          begin match codecs with
          | ListCodecs.Bytes8 (decode, encode)
          | ListCodecs.Struct { ListCodecs.bytes = (decode, encode); _ } ->
              let get ls i = decode (make_element_slice ls i 8) in
              let set ls i v = encode v (make_element_slice ls i 8) in
              (get, set)
          | _ ->
              invalid_msg
                "decoded List<8 byte> where a different list type was expected"
          end
      | ListStorageType.Pointer ->
          begin match codecs with
          | ListCodecs.Pointer (decode, encode)
          | ListCodecs.Struct { ListCodecs.pointer = (decode, encode); _ } ->
              let get ls i = decode (make_element_slice ls i sizeof_uint64) in
              let set ls i v = encode v (make_element_slice ls i sizeof_uint64) in
              (get, set)
          | _ ->
              invalid_msg
                "decoded List<pointer> where a different list type was expected"
          end
      | ListStorageType.Composite (data_words, pointer_words) ->
          let data_size     = data_words * sizeof_uint64 in
          let pointers_size = pointer_words * sizeof_uint64 in
          let total_size    = data_size + pointers_size in
          let make_storage ls i =
            (* Skip over the composite tag word *)
            let content_offset =
              ls.ListStorage.storage.Slice.start + sizeof_uint64
            in
            let data = {
              ls.ListStorage.storage with
              Slice.start = content_offset + (i * total_size);
              Slice.len = data_size;
            } in
            let pointers = {
              data with
              Slice.start = Slice.get_end data;
              Slice.len   = pointers_size;
            } in
            { StructStorage.data; StructStorage.pointers; }
          in
          let make_bytes_handlers ~size ~decode ~encode =
            if data_words = 0 then
              invalid_msg
                "decoded List<composite> with empty data region where data was expected"
            else
              let get ls i =
                let struct_storage = make_storage ls i in
                let slice = {
                  struct_storage.StructStorage.data with
                  Slice.len = size
                } in
                decode slice
              in
              let set ls i v =
                let struct_storage = make_storage ls i in
                let slice = {
                  struct_storage.StructStorage.data with
                  Slice.len = size
                } in
                encode v slice
              in
              (get, set)
          in
          begin match codecs with
          | ListCodecs.Empty (decode, encode) ->
              let get ls i   = decode () in
              let set ls i v = encode v in
              (get, set)
          | ListCodecs.Bit (decode, encode) ->
              if data_words = 0 then
                invalid_msg
                  "decoded List<composite> with empty data region where data was expected"
              else
                let get ls i =
                  let struct_storage = make_storage ls i in
                  let first_byte = Slice.get_uint8 struct_storage.StructStorage.data 0 in
                  let is_set = (first_byte land 0x1) <> 0 in
                  decode is_set
                in
                let set ls i v =
                  let struct_storage = make_storage ls i in
                  let first_byte =
                    Slice.get_uint8 struct_storage.StructStorage.data 0
                  in
                  let first_byte =
                    if encode v then first_byte lor 0x1 else first_byte land 0xfe
                  in
                  Slice.set_uint8 struct_storage.StructStorage.data 0 first_byte
                in
                (get, set)
          | ListCodecs.Bytes1 (decode, encode) ->
              make_bytes_handlers ~size:1 ~decode ~encode
          | ListCodecs.Bytes2 (decode, encode) ->
              make_bytes_handlers ~size:2 ~decode ~encode
          | ListCodecs.Bytes4 (decode, encode) ->
              make_bytes_handlers ~size:4 ~decode ~encode
          | ListCodecs.Bytes8 (decode, encode) ->
              make_bytes_handlers ~size:8 ~decode ~encode
          | ListCodecs.Pointer (decode, encode) ->
              if pointer_words = 0 then
                invalid_msg
                  "decoded List<composite> with empty pointers region where \
                   pointers were expected"
              else
                let get ls i =
                  let struct_storage = make_storage ls i in
                  let slice = {
                    struct_storage.StructStorage.pointers with
                    Slice.len = sizeof_uint64
                  } in
                  decode slice
                in
                let set ls i v =
                  let struct_storage = make_storage ls i in
                  let slice = {
                    struct_storage.StructStorage.pointers with
                    Slice.len = sizeof_uint64
                  } in
                  encode v slice
                in
                (get, set)
          | ListCodecs.Struct { ListCodecs.composite = (decode, encode); _ } ->
              let get ls i = decode (make_storage ls i) in
              let set ls i v = encode v (make_storage ls i) in
              (get, set)
          end
    in {
      InnerArray.length;
      InnerArray.init;
      InnerArray.get_unsafe;
      InnerArray.set_unsafe;
      InnerArray.storage = Some list_storage;
    }


  (* Given list storage which is expected to contain UInt8 data, decode the data as
     an OCaml string. *)
  let string_of_uint8_list
      ~(null_terminated : bool)   (* true if the data is expected to end in 0 *)
      (list_storage : 'cap ListStorage.t)
    : string =
    let open ListStorage in
    match list_storage.storage_type with
    | ListStorageType.Bytes1 ->
        let result_byte_count =
          if null_terminated then
            let () =
              if list_storage.num_elements < 1 then
                invalid_msg "empty string list has no space for null terminator"
            in
            let terminator =
              Slice.get_uint8 list_storage.storage (list_storage.num_elements - 1)
            in
            let () = if terminator <> 0 then
              invalid_msg "string list is not null terminated"
            in
            list_storage.num_elements - 1
          else
            list_storage.num_elements
        in
        let buf = Bytes.create result_byte_count in
        let () =
          for i = 0 to result_byte_count - 1 do
            Bytes.set buf i (Char.of_int_exn (Slice.get_uint8 list_storage.storage i))
          done
        in
        Bytes.to_string buf
    | _ ->
        invalid_msg "decoded non-UInt8 list where string data was expected"


  let struct_of_bytes_slice slice =
    let data = slice in
    let pointers = {
      slice with
      Slice.start = Slice.get_end data;
      Slice.len   = 0;
    } in
    { StructStorage.data; StructStorage.pointers }

  let struct_of_pointer_slice slice =
    let () = assert (slice.Slice.len = sizeof_uint64) in
    let data = {
      slice with
      Slice.len = 0
    } in
    let pointers = {
      slice with
      Slice.len = sizeof_uint64;
    } in
    { StructStorage.data; StructStorage.pointers }


  (* Given some list storage corresponding to a struct list, construct
     a function for mapping an element index to the associated
     struct storage. *)
  let make_struct_of_list_index list_storage =
    let storage      = list_storage.ListStorage.storage in
    let storage_type = list_storage.ListStorage.storage_type in
    match list_storage.ListStorage.storage_type with
    | ListStorageType.Empty ->
        fun i ->
          let slice = {
            storage with
            Slice.start = storage.Slice.start;
            Slice.len   = 0;
          } in
          struct_of_bytes_slice slice
    | ListStorageType.Bytes1
    | ListStorageType.Bytes2
    | ListStorageType.Bytes4
    | ListStorageType.Bytes8 ->
        (* Short data-only struct *)
        let byte_count = ListStorageType.get_byte_count storage_type in
        fun i ->
          let slice = {
            storage with
            Slice.start = storage.Slice.start + (i * byte_count);
            Slice.len   = byte_count;
          } in
          struct_of_bytes_slice slice
    | ListStorageType.Pointer ->
        (* Single-pointer struct *)
        fun i ->
          let slice = {
            storage with
            Slice.start = (storage.Slice.start) + (i * sizeof_uint64);
            Slice.len   = sizeof_uint64;
          } in
          struct_of_pointer_slice slice
    | ListStorageType.Composite (data_words, pointer_words) ->
        let data_size     = data_words * sizeof_uint64 in
        let pointers_size = pointer_words * sizeof_uint64 in
        let element_size  = data_size + pointers_size in
        (* Skip over the composite tag word *)
        let content_offset = storage.Slice.start + sizeof_uint64 in
        fun i ->
          let data = {
            storage with
            Slice.start = content_offset + (i * element_size);
            Slice.len   = data_size;
          } in
          let pointers = {
            storage with
            Slice.start = Slice.get_end data;
            Slice.len   = pointers_size;
          } in
          { StructStorage.data; StructStorage.pointers }
    | ListStorageType.Bit ->
        invalid_msg "decoded List<Bool> where List<composite> was expected"

end

