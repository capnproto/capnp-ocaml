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

(* Runtime support for Builder interfaces.  In many ways this parallels the
   Reader support, to the point of using the same function names; however,
   the underlying message must be tagged as read/write, and many functions in
   this module may allocate message space (for example, dereferencing a struct
   pointer will cause struct storage to be immediately allocated if that pointer
   was null). *)

open Core.Std

let sizeof_uint64 = Common.sizeof_uint64

module Make (MessageWrapper : Message.S) = struct
  module RReader = Reader.Make(MessageWrapper)
  module RC = Common.Make(MessageWrapper)
  include RC

  module BOps = BuilderOps.Make(MessageWrapper)(MessageWrapper)


  (* Given a string, generate an orphaned cap'n proto List<Uint8> which contains
     the string content. *)
  let uint8_list_of_string
      ~(null_terminated : bool)   (* true if the data is expected to end in 0 *)
      ~(dest_message : rw Message.t)
      (src : string)
    : rw ListStorage.t =
    let list_storage = BOps.alloc_list_storage dest_message
        Common.ListStorageType.Bytes1
        (String.length src + (if null_terminated then 1 else 0))
    in
    let (_ : int) = String.fold src ~init:0 ~f:(fun ofs c ->
        let byte = Char.to_int c in
        let () = Slice.set_uint8 list_storage.ListStorage.storage ofs byte in
        ofs + 1)
    in
    list_storage


  let void_list_codecs = ListCodecs.Empty (
      (fun (x : unit) -> x), (fun (x : unit) -> x))

  let bit_list_codecs = ListCodecs.Bit (
      (fun (x : bool) -> x), (fun (x : bool) -> x))

  let int8_list_codecs = ListCodecs.Bytes1 (
      (fun slice -> Slice.get_int8 slice 0),
        (fun v slice -> Slice.set_int8 slice 0 v))

  let int16_list_codecs = ListCodecs.Bytes2 (
      (fun slice -> Slice.get_int16 slice 0),
        (fun v slice -> Slice.set_int16 slice 0 v))

  let int32_list_codecs = ListCodecs.Bytes4 (
      (fun slice -> Slice.get_int32 slice 0),
        (fun v slice -> Slice.set_int32 slice 0 v))

  let int64_list_codecs = ListCodecs.Bytes8 (
      (fun slice -> Slice.get_int64 slice 0),
        (fun v slice -> Slice.set_int64 slice 0 v))

  let uint8_list_codecs = ListCodecs.Bytes1 (
      (fun slice -> Slice.get_uint8 slice 0),
        (fun v slice -> Slice.set_uint8 slice 0 v))

  let uint16_list_codecs = ListCodecs.Bytes2 (
      (fun slice -> Slice.get_uint16 slice 0),
        (fun v slice -> Slice.set_uint16 slice 0 v))

  let uint32_list_codecs = ListCodecs.Bytes4 (
      (fun slice -> Slice.get_uint32 slice 0),
        (fun v slice -> Slice.set_uint32 slice 0 v))

  let uint64_list_codecs = ListCodecs.Bytes8 (
      (fun slice -> Slice.get_uint64 slice 0),
        (fun v slice -> Slice.set_uint64 slice 0 v))

  let float32_list_codecs = ListCodecs.Bytes4 (
      (fun slice -> Int32.float_of_bits (Slice.get_int32 slice 0)),
        (fun v slice -> Slice.set_int32 slice 0
          (Int32.bits_of_float v)))

  let float64_list_codecs = ListCodecs.Bytes8 (
      (fun slice -> Int64.float_of_bits (Slice.get_int64 slice 0)),
        (fun v slice -> Slice.set_int64 slice 0
          (Int64.bits_of_float v)))

  let text_list_codecs =
    let decode slice =
      (* Text fields are always accessed by value, not by reference, since
         we always do an immediate decode to [string].  Therefore we can
         use the Reader logic to handle this case. *)
      match RReader.deref_list_pointer slice with
      | Some list_storage ->
          string_of_uint8_list ~null_terminated:true list_storage
      | None ->
          ""
    in
    let encode s slice =
      let new_list_storage = uint8_list_of_string ~null_terminated:true
          ~dest_message:slice.Slice.msg s
      in
      BOps.init_list_pointer slice new_list_storage
    in
    ListCodecs.Pointer (decode, encode)

  let blob_list_codecs =
    let decode slice =
      (* Data fields are always accessed by value, not by reference, since
         we always do an immediate decode to [string].  Therefore we can
         use the Reader logic to handle this case. *)
      match RReader.deref_list_pointer slice with
      | Some list_storage ->
          string_of_uint8_list ~null_terminated:false list_storage
      | None ->
          ""
    in
    let encode s slice =
      let new_list_storage = uint8_list_of_string ~null_terminated:false
          ~dest_message:slice.Slice.msg s
      in
      BOps.init_list_pointer slice new_list_storage
    in
    ListCodecs.Pointer (decode, encode)

  let struct_list_codecs =
    let bytes_decoder slice =
      RC.struct_of_bytes_slice slice
    in
    let bytes_encoder v slice =
      let dest = RC.struct_of_bytes_slice slice in
      BOps.deep_copy_struct_to_dest ~src:v ~dest
    in
    let pointer_decoder slice =
      RC.struct_of_pointer_slice slice
    in
    let pointer_encoder v slice =
      let dest = RC.struct_of_pointer_slice slice in
      BOps.deep_copy_struct_to_dest ~src:v ~dest
    in
    let composite_decoder x = x in
    let composite_encoder v dest = BOps.deep_copy_struct_to_dest ~src:v ~dest in
    ListCodecs.Struct {
      ListCodecs.bytes     = (bytes_decoder, bytes_encoder);
      ListCodecs.pointer   = (pointer_decoder, pointer_encoder);
      ListCodecs.composite = (composite_decoder, composite_encoder);
    }


  (*******************************************************************************
   * METHODS FOR GETTING OBJECTS STORED BY VALUE
   *******************************************************************************)

  module Discr = struct
    type t = {
      value    : int;
      byte_ofs : int;
    }
  end

  let rec set_opt_discriminant
      (data : rw Slice.t)
      (discr : Discr.t option)
    : unit =
    match discr with
    | None ->
        ()
    | Some x ->
        set_uint16 data ~default:0 ~byte_ofs:x.Discr.byte_ofs x.Discr.value

  and set_uint16
      ?(discr : Discr.t option)
      (data : rw Slice.t)
      ~(default : int)
      ~(byte_ofs : int)
      (value : int)
    : unit =
    let () = set_opt_discriminant data discr in
    Slice.set_uint16 data byte_ofs (value lxor default)

  (* Given storage for a struct, get the bytes associated with the
     struct data section.  The provided decoding function is applied
     to the resulting region.  If the optional discriminant parameter
     is supplied, then the discriminant is also set as a side-effect. *)
  (* FIXME: [apply_data_field] would be a better name *)
  let get_data_field
      ?(discr : Discr.t option)
      (struct_storage : rw StructStorage.t)
      ~(f : rw Slice.t -> 'a)
    : 'a =
    let data = struct_storage.StructStorage.data in
    let result = f data in
    let () = set_opt_discriminant data discr in
    result

  let get_void
      (data : 'cap Slice.t)
    : unit =
    ()

   let get_bit
      ~(default : bool)
      ~(byte_ofs : int)
      ~(bit_ofs : int)
      (data : 'cap Slice.t)
     : bool =
     let byte_val = Slice.get_uint8 data byte_ofs in
     let bit_val = (byte_val land (1 lsl bit_ofs)) <> 0 in
     if default then not bit_val else bit_val

  let get_int8
      ~(default : int)
      ~(byte_ofs : int)
      (data : 'cap Slice.t)
    : int =
    let numeric = Slice.get_int8 data byte_ofs in
    numeric lxor default

  let get_int16
      ~(default : int)
      ~(byte_ofs : int)
      (data : 'cap Slice.t)
    : int =
    let numeric = Slice.get_int16 data byte_ofs in
    numeric lxor default

  let get_int32
      ~(default : int32)
      ~(byte_ofs : int)
      (data : 'cap Slice.t)
      : int32 =
    let numeric = Slice.get_int32 data byte_ofs in
    Int32.bit_xor numeric default

  let get_int64
      ~(default : int64)
      ~(byte_ofs : int)
      (data : 'cap Slice.t)
    : int64 =
    let numeric = Slice.get_int64 data byte_ofs in
    Int64.bit_xor numeric default

  let get_uint8
      ~(default : int)
      ~(byte_ofs : int)
      (data : 'cap Slice.t)
    : int =
    let numeric = Slice.get_uint8 data byte_ofs in
    numeric lxor default

  let get_uint16
      ~(default : int)
      ~(byte_ofs : int)
      (data : 'cap Slice.t)
    : int =
    let numeric = Slice.get_uint16 data byte_ofs in
    numeric lxor default

  let get_uint32
      ~(default : Uint32.t)
      ~(byte_ofs : int)
      (data : 'cap Slice.t)
      : Uint32.t =
    let numeric = Slice.get_uint32 data byte_ofs in
    Uint32.logxor numeric default

  let get_uint64
      ~(default : Uint64.t)
      ~(byte_ofs : int)
      (data : 'cap Slice.t)
    : Uint64.t =
    let numeric = Slice.get_uint64 data byte_ofs in
    Uint64.logxor numeric default

  let get_float32
      ~(default_bits : int32)
      ~(byte_ofs : int)
      (data : 'cap Slice.t)
    : float =
    let numeric = Slice.get_int32 data byte_ofs in
    let bits = Int32.bit_xor numeric default_bits in
    Int32.float_of_bits bits

  let get_float64
      ~(default_bits : int64)
      ~(byte_ofs : int)
      (data : 'cap Slice.t)
    : float =
    let numeric = Slice.get_int64 data byte_ofs in
    let bits = Int64.bit_xor numeric default_bits in
    Int64.float_of_bits bits


  (*******************************************************************************
   * METHODS FOR SETTING OBJECTS STORED BY VALUE
   *******************************************************************************)

  let set_void
      (data : 'cap Slice.t)
    : unit =
    ()

  let set_bit
      ~(default : bool)
      ~(byte_ofs : int)
      ~(bit_ofs : int)
      (value : bool)
      (data : rw Slice.t)
    : unit =
    let default_bit = if default then 1 else 0 in
    let value_bit = if value then 1 else 0 in
    let stored_bit = default_bit lxor value_bit in
    let byte_val = Slice.get_uint8 data byte_ofs in
    let byte_val = byte_val land (lnot (1 lsl bit_ofs)) in
    let byte_val = byte_val lor (stored_bit lsl bit_ofs) in
    Slice.set_uint8 data byte_ofs byte_val

  let set_int8
      ~(default : int)
      ~(byte_ofs : int)
      (value : int)
      (data : rw Slice.t)
    : unit =
    Slice.set_int8 data byte_ofs (value lxor default)

  let set_int16
      ~(default : int)
      ~(byte_ofs : int)
      (value : int)
      (data : rw Slice.t)
    : unit =
    Slice.set_int16 data byte_ofs (value lxor default)

  let set_int32
      ~(default : int32)
      ~(byte_ofs : int)
      (value : int32)
      (data : rw Slice.t)
    : unit =
    Slice.set_int32 data byte_ofs
      (Int32.bit_xor value default)

  let set_int64
      ~(default : int64)
      ~(byte_ofs : int)
      (value : int64)
      (data : rw Slice.t)
    : unit =
    Slice.set_int64 data byte_ofs
      (Int64.bit_xor value default)

  let set_uint8
      ~(default : int)
      ~(byte_ofs : int)
      (value : int)
      (data : rw Slice.t)
    : unit =
    Slice.set_uint8 data byte_ofs (value lxor default)

  let set_uint16
      ~(default : int)
      ~(byte_ofs : int)
      (value : int)
      (data : rw Slice.t)
    : unit =
    Slice.set_uint16 data byte_ofs (value lxor default)

  let set_uint32
      ~(default : Uint32.t)
      ~(byte_ofs : int)
      (value : Uint32.t)
      (data : rw Slice.t)
    : unit =
    Slice.set_uint32 data byte_ofs
      (Uint32.logxor value default)

  let set_uint64
      ~(default : Uint64.t)
      ~(byte_ofs : int)
      (value : Uint64.t)
      (data : rw Slice.t)
    : unit =
    Slice.set_uint64 data byte_ofs
      (Uint64.logxor value default)

  let set_float32
      ~(default_bits : int32)
      ~(byte_ofs : int)
      (value : float)
      (data : rw Slice.t)
    : unit =
    Slice.set_int32 data byte_ofs
      (Int32.bit_xor (Int32.bits_of_float value) default_bits)

  let set_float64
      ~(default_bits : int64)
      ~(byte_ofs : int)
      (value : float)
      (data : rw Slice.t)
    : unit =
    Slice.set_int64 data byte_ofs
      (Int64.bit_xor (Int64.bits_of_float value) default_bits)


  (*******************************************************************************
   * METHODS FOR GETTING OBJECTS STORED BY POINTER
   *******************************************************************************)


  (* Given storage for a struct, get the bytes associated with struct
     pointer at offset [pointer_word].  The provided decoding function
     is applied to the resulting region.  If the optional discriminant
     parameter is supplied, then the discriminant is also set as a
     side-effect. *)
  (* FIXME: [apply_pointer_field] would be a better name *)
  let get_pointer_field
      ?(discr : Discr.t option)
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      ~(f : 'cap Slice.t -> 'a)
    : 'a =
    let result =
      f (BOps.get_struct_pointer struct_storage pointer_word)
    in
    let () = set_opt_discriminant struct_storage.StructStorage.data discr in
    result

  let get_text
      ~(default : string)
      (pointer_bytes : 'cap Slice.t)
    : string =
    (* Text fields are always accessed by value, not by reference, since
       we always do an immediate decode to [string].  Therefore we can
       use the Reader logic to handle this case. *)
    match RReader.deref_list_pointer pointer_bytes with
    | Some list_storage ->
        string_of_uint8_list ~null_terminated:true list_storage
    | None ->
        default

  let get_blob
      ~(default : string)
      (pointer_bytes : 'cap Slice.t)
    : string =
    (* Data fields are always accessed by value, not by reference, since
       we always do an immediate decode to [string].  Therefore we can
       use the Reader logic to handle this case. *)
    match RReader.deref_list_pointer pointer_bytes with
    | Some list_storage ->
        string_of_uint8_list ~null_terminated:false list_storage
    | None ->
        default


  let get_list
      ?(struct_sizes : BuilderOps.StructSizes.t option)
      ?(default : ro ListStorage.t option)
      ~(storage_type : Common.ListStorageType.t)
      ~(codecs : 'a ListCodecs.t)
      (pointer_bytes : rw Slice.t)
    : (rw, 'a, rw ListStorage.t) InnerArray.t =
    let create_default message =
      match default with
      | Some default_storage ->
          BOps.deep_copy_list ?struct_sizes ~src:default_storage ~dest_message:message ()
      | None ->
          BOps.alloc_list_storage message storage_type 0
    in
    let list_storage = BOps.deref_list_pointer ?struct_sizes ~create_default
        pointer_bytes
    in
    make_array_readwrite list_storage codecs

  let get_void_list
      ?(default : ro ListStorage.t option)
      (pointer_bytes : rw Slice.t)
    : (rw, unit, rw ListStorage.t) InnerArray.t =
    get_list ?default ~storage_type:Common.ListStorageType.Empty
      ~codecs:void_list_codecs pointer_bytes

  let get_bit_list
      ?(default : ro ListStorage.t option)
      (pointer_bytes : rw Slice.t)
    : (rw, bool, rw ListStorage.t) InnerArray.t =
    get_list ?default ~storage_type:Common.ListStorageType.Bit
      ~codecs:bit_list_codecs pointer_bytes

  let get_int8_list
      ?(default : ro ListStorage.t option)
      (pointer_bytes : rw Slice.t)
    : (rw, int, rw ListStorage.t) InnerArray.t =
    get_list ?default ~storage_type:Common.ListStorageType.Bytes1
      ~codecs:int8_list_codecs pointer_bytes

  let get_int16_list
      ?(default : ro ListStorage.t option)
      (pointer_bytes : rw Slice.t)
    : (rw, int, rw ListStorage.t) InnerArray.t =
    get_list ?default ~storage_type:Common.ListStorageType.Bytes2
      ~codecs:int16_list_codecs pointer_bytes

  let get_int32_list
      ?(default : ro ListStorage.t option)
      (pointer_bytes : rw Slice.t)
    : (rw, int32, rw ListStorage.t) InnerArray.t =
    get_list ?default ~storage_type:Common.ListStorageType.Bytes4
      ~codecs:int32_list_codecs pointer_bytes

  let get_int64_list
      ?(default : ro ListStorage.t option)
      (pointer_bytes : rw Slice.t)
    : (rw, int64, rw ListStorage.t) InnerArray.t =
    get_list ?default ~storage_type:Common.ListStorageType.Bytes8
      ~codecs:int64_list_codecs pointer_bytes

  let get_uint8_list
      ?(default : ro ListStorage.t option)
      (pointer_bytes : rw Slice.t)
    : (rw, int, rw ListStorage.t) InnerArray.t =
    get_list ?default ~storage_type:Common.ListStorageType.Bytes1
      ~codecs:uint8_list_codecs pointer_bytes

  let get_uint16_list
      ?(default : ro ListStorage.t option)
      (pointer_bytes : rw Slice.t)
    : (rw, int, rw ListStorage.t) InnerArray.t =
    get_list ?default ~storage_type:Common.ListStorageType.Bytes2
      ~codecs:uint16_list_codecs pointer_bytes

  let get_uint32_list
      ?(default : ro ListStorage.t option)
      (pointer_bytes : rw Slice.t)
    : (rw, Uint32.t, rw ListStorage.t) InnerArray.t =
    get_list ?default ~storage_type:Common.ListStorageType.Bytes4
      ~codecs:uint32_list_codecs pointer_bytes

  let get_uint64_list
      ?(default : ro ListStorage.t option)
      (pointer_bytes : rw Slice.t)
    : (rw, Uint64.t, rw ListStorage.t) InnerArray.t =
    get_list ?default ~storage_type:Common.ListStorageType.Bytes8
      ~codecs:uint64_list_codecs pointer_bytes

  let get_float32_list
      ?(default : ro ListStorage.t option)
      (pointer_bytes : rw Slice.t)
    : (rw, float, rw ListStorage.t) InnerArray.t =
    get_list ?default ~storage_type:Common.ListStorageType.Bytes4
      ~codecs:float32_list_codecs pointer_bytes

  let get_float64_list
      ?(default : ro ListStorage.t option)
      (pointer_bytes : rw Slice.t)
    : (rw, float, rw ListStorage.t) InnerArray.t =
    get_list ?default ~storage_type:Common.ListStorageType.Bytes8
      ~codecs:float64_list_codecs pointer_bytes

  let get_text_list
      ?(default : ro ListStorage.t option)
      (pointer_bytes : rw Slice.t)
    : (rw, string, rw ListStorage.t) InnerArray.t =
    get_list ?default ~storage_type:Common.ListStorageType.Pointer
      ~codecs:text_list_codecs pointer_bytes

  let get_blob_list
      ?(default : ro ListStorage.t option)
      (pointer_bytes : rw Slice.t)
    : (rw, string, rw ListStorage.t) InnerArray.t =
    get_list ?default ~storage_type:Common.ListStorageType.Pointer
      ~codecs:blob_list_codecs pointer_bytes

  let get_struct_list
      ?(default : ro ListStorage.t option)
      ~(data_words : int)
      ~(pointer_words : int)
      (pointer_bytes : rw Slice.t)
    : (rw, rw StructStorage.t, rw ListStorage.t) InnerArray.t =
    get_list ~struct_sizes:{
      BuilderOps.StructSizes.data_words;
      BuilderOps.StructSizes.pointer_words }
      ?default ~storage_type:(
        Common.ListStorageType.Composite (data_words, pointer_words))
      ~codecs:struct_list_codecs pointer_bytes

  let get_struct
      ?(default : ro StructStorage.t option)
      ~(data_words : int)
      ~(pointer_words : int)
      (pointer_bytes : rw Slice.t)
    : rw StructStorage.t =
    let create_default message =
      match default with
      | Some default_storage ->
          BOps.deep_copy_struct ~src:default_storage ~dest_message:message
            ~data_words ~pointer_words
      | None ->
          BOps.alloc_struct_storage message ~data_words ~pointer_words
    in
    BOps.deref_struct_pointer ~create_default ~data_words ~pointer_words pointer_bytes


  (*******************************************************************************
   * METHODS FOR SETTING OBJECTS STORED BY POINTER
   *******************************************************************************)

  let set_text
      (value : string)
      (pointer_bytes : rw Slice.t)
    : unit =
    let new_string_storage = uint8_list_of_string
      ~null_terminated:true ~dest_message:pointer_bytes.Slice.msg
      value
    in
    let () = BOps.deep_zero_pointer pointer_bytes in
    BOps.init_list_pointer pointer_bytes new_string_storage

  let set_blob
      (value : string)
      (pointer_bytes : rw Slice.t)
    : unit =
    let new_string_storage = uint8_list_of_string
      ~null_terminated:false ~dest_message:pointer_bytes.Slice.msg
      value
    in
    let () = BOps.deep_zero_pointer pointer_bytes in
    BOps.init_list_pointer pointer_bytes new_string_storage

  let set_list_from_storage
      ?(struct_sizes : BuilderOps.StructSizes.t option)
      ~(storage_type : Common.ListStorageType.t)
      ~(codecs : 'a ListCodecs.t)
      (value : 'cap ListStorage.t option)
      (pointer_bytes : rw Slice.t)
    : (rw, 'a, rw ListStorage.t) InnerArray.t =
    let dest_storage =
      match value with
      | Some src_storage ->
          BOps.deep_copy_list ?struct_sizes
            ~src:src_storage ~dest_message:pointer_bytes.Slice.msg ()
      | None ->
          BOps.alloc_list_storage pointer_bytes.Slice.msg storage_type 0
    in
    let () = BOps.deep_zero_pointer pointer_bytes in
    let () = BOps.init_list_pointer pointer_bytes dest_storage in
    make_array_readwrite dest_storage codecs

  let set_list
      ?(struct_sizes : BuilderOps.StructSizes.t option)
      ~(storage_type : Common.ListStorageType.t)
      ~(codecs : 'a ListCodecs.t)
      (value : ('cap1, 'a, 'cap2 ListStorage.t) InnerArray.t)
      (pointer_bytes : rw Slice.t)
    : (rw, 'a, rw ListStorage.t) InnerArray.t =
    set_list_from_storage ?struct_sizes ~storage_type ~codecs
      (InnerArray.to_storage value)
      pointer_bytes

  let set_void_list
      (value : ('cap1, unit, 'cap2 ListStorage.t) InnerArray.t)
      (pointer_bytes : rw Slice.t)
    : (rw, unit, rw ListStorage.t) InnerArray.t =
    set_list ~storage_type:Common.ListStorageType.Empty ~codecs:void_list_codecs
      value pointer_bytes

  let set_bit_list
      (value : ('cap1, bool, 'cap2 ListStorage.t) InnerArray.t)
      (pointer_bytes : rw Slice.t)
    : (rw, bool, rw ListStorage.t) InnerArray.t =
    set_list ~storage_type:Common.ListStorageType.Bit ~codecs:bit_list_codecs
      value pointer_bytes

  let set_int8_list
      (value : ('cap1, int, 'cap2 ListStorage.t) InnerArray.t)
      (pointer_bytes : rw Slice.t)
    : (rw, int, 'cap ListStorage.t) InnerArray.t =
    set_list ~storage_type:Common.ListStorageType.Bytes1 ~codecs:int8_list_codecs
      value pointer_bytes

  let set_int16_list
      (value : ('cap1, int, 'cap2 ListStorage.t) InnerArray.t)
      (pointer_bytes : rw Slice.t)
    : (rw, int, 'cap ListStorage.t) InnerArray.t =
    set_list ~storage_type:Common.ListStorageType.Bytes2 ~codecs:int16_list_codecs
      value pointer_bytes

  let set_int32_list
      (value : ('cap1, int32, 'cap ListStorage.t) InnerArray.t)
      (pointer_bytes : rw Slice.t)
    : (rw, int32, rw ListStorage.t) InnerArray.t =
    set_list ~storage_type:Common.ListStorageType.Bytes4 ~codecs:int32_list_codecs
      value pointer_bytes

  let set_int64_list
      (value : ('cap1, int64, 'cap2 ListStorage.t) InnerArray.t)
      (pointer_bytes : rw Slice.t)
    : (rw, int64, rw ListStorage.t) InnerArray.t =
    set_list ~storage_type:Common.ListStorageType.Bytes8 ~codecs:int64_list_codecs
      value pointer_bytes

  let set_uint8_list
      (value : ('cap1, int, 'cap2 ListStorage.t) InnerArray.t)
      (pointer_bytes : rw Slice.t)
    : (rw, int, rw ListStorage.t) InnerArray.t =
    set_list ~storage_type:Common.ListStorageType.Bytes1 ~codecs:uint8_list_codecs
      value pointer_bytes

  let set_uint16_list
      (value : ('cap1, int, 'cap2 ListStorage.t) InnerArray.t)
      (pointer_bytes : rw Slice.t)
    : (rw, int, rw ListStorage.t) InnerArray.t =
    set_list ~storage_type:Common.ListStorageType.Bytes2 ~codecs:uint16_list_codecs
      value pointer_bytes

  let set_uint32_list
      (value : ('cap1, Uint32.t, 'cap2 ListStorage.t) InnerArray.t)
      (pointer_bytes : rw Slice.t)
    : (rw, Uint32.t, rw ListStorage.t) InnerArray.t =
    set_list ~storage_type:Common.ListStorageType.Bytes4 ~codecs:uint32_list_codecs
      value pointer_bytes

  let set_uint64_list
      (value : ('cap1, Uint64.t, 'cap2 ListStorage.t) InnerArray.t)
      (pointer_bytes : rw Slice.t)
    : (rw, Uint64.t, rw ListStorage.t) InnerArray.t =
    set_list ~storage_type:Common.ListStorageType.Bytes8 ~codecs:uint64_list_codecs
      value pointer_bytes

  let set_float32_list
      (value : ('cap1, float, 'cap2 ListStorage.t) InnerArray.t)
      (pointer_bytes : rw Slice.t)
    : (rw, float, rw ListStorage.t) InnerArray.t =
    set_list ~storage_type:Common.ListStorageType.Bytes4 ~codecs:float32_list_codecs
      value pointer_bytes

  let set_float64_list
      (value : ('cap1, float, 'cap2 ListStorage.t) InnerArray.t)
      (pointer_bytes : rw Slice.t)
    : (rw, float, rw ListStorage.t) InnerArray.t =
    set_list ~storage_type:Common.ListStorageType.Bytes8 ~codecs:float64_list_codecs
      value pointer_bytes

  let set_text_list
      (value : ('cap1, string, 'cap2 ListStorage.t) InnerArray.t)
      (pointer_bytes : rw Slice.t)
    : (rw, string, rw ListStorage.t) InnerArray.t =
    set_list ~storage_type:Common.ListStorageType.Pointer ~codecs:text_list_codecs
      value pointer_bytes

  let set_blob_list
      (value : ('cap1, string, 'cap2 ListStorage.t) InnerArray.t)
      (pointer_bytes : rw Slice.t)
    : (rw, string, rw ListStorage.t) InnerArray.t =
    set_list ~storage_type:Common.ListStorageType.Pointer ~codecs:blob_list_codecs
      value pointer_bytes

  let set_struct_list
      ~(data_words : int)
      ~(pointer_words : int)
      (* FIXME: this won't allow assignment from Reader struct lists *)
      (value : ('cap1, 'cap2 StructStorage.t, 'cap2 ListStorage.t) InnerArray.t)
      (pointer_bytes : rw Slice.t)
    : (rw, rw StructStorage.t, rw ListStorage.t) InnerArray.t =
    set_list ~struct_sizes:{
      BuilderOps.StructSizes.data_words;
      BuilderOps.StructSizes.pointer_words }
      ~storage_type:(Common.ListStorageType.Composite (data_words, pointer_words))
      ~codecs:struct_list_codecs value pointer_bytes

  let set_struct
      ~(data_words : int)
      ~(pointer_words : int)
      (value : 'cap StructStorage.t option)
      (pointer_bytes : rw Slice.t)
    : rw StructStorage.t =
    let dest_storage =
      match value with
      | Some src_storage ->
          BOps.deep_copy_struct ~src:src_storage
            ~dest_message:pointer_bytes.Slice.msg ~data_words ~pointer_words
      | None ->
          BOps.alloc_struct_storage pointer_bytes.Slice.msg ~data_words ~pointer_words
    in
    let () = BOps.deep_zero_pointer pointer_bytes in
    let () = BOps.init_struct_pointer pointer_bytes dest_storage in
    dest_storage


  (*******************************************************************************
   * METHODS FOR INITIALIZING OBJECTS STORED BY POINTER
   *******************************************************************************)

  let init_blob
      (num_elements : int)
      (pointer_bytes : rw Slice.t)
    : unit =
    let s = String.make num_elements '\x00' in
    set_blob s pointer_bytes

  let init_list
      ~(storage_type : Common.ListStorageType.t)
      ~(codecs : 'a ListCodecs.t)
      (num_elements : int)
      (pointer_bytes : rw Slice.t)
    : (rw, 'a, rw ListStorage.t) InnerArray.t =
    let () = BOps.deep_zero_pointer pointer_bytes in
    let message = pointer_bytes.Slice.msg in
    let list_storage = BOps.alloc_list_storage message storage_type num_elements in
    let () = BOps.init_list_pointer pointer_bytes list_storage in
    make_array_readwrite list_storage codecs

  let init_void_list
      (num_elements : int)
      (pointer_bytes : rw Slice.t)
    : (rw, unit, rw ListStorage.t) InnerArray.t =
    init_list ~storage_type:Common.ListStorageType.Empty ~codecs:void_list_codecs
      num_elements pointer_bytes

  let init_bit_list
      (num_elements : int)
      (pointer_bytes : rw Slice.t)
    : (rw, bool, rw ListStorage.t) InnerArray.t =
    init_list ~storage_type:Common.ListStorageType.Bit ~codecs:bit_list_codecs
      num_elements pointer_bytes

  let init_int8_list
      (num_elements : int)
      (pointer_bytes : rw Slice.t)
    : (rw, int, rw ListStorage.t) InnerArray.t =
    init_list ~storage_type:Common.ListStorageType.Bytes1 ~codecs:int8_list_codecs
      num_elements pointer_bytes

  let init_int16_list
      (num_elements : int)
      (pointer_bytes : rw Slice.t)
    : (rw, int, rw ListStorage.t) InnerArray.t =
    init_list ~storage_type:Common.ListStorageType.Bytes2 ~codecs:int16_list_codecs
      num_elements pointer_bytes

  let init_int32_list
      (num_elements : int)
      (pointer_bytes : rw Slice.t)
    : (rw, int32, rw ListStorage.t) InnerArray.t =
    init_list ~storage_type:Common.ListStorageType.Bytes4 ~codecs:int32_list_codecs
      num_elements pointer_bytes

  let init_int64_list
      (num_elements : int)
      (pointer_bytes : rw Slice.t)
    : (rw, int64, rw ListStorage.t) InnerArray.t =
    init_list ~storage_type:Common.ListStorageType.Bytes8 ~codecs:int64_list_codecs
      num_elements pointer_bytes

  let init_uint8_list
      (num_elements : int)
      (pointer_bytes : rw Slice.t)
    : (rw, int, rw ListStorage.t) InnerArray.t =
    init_list ~storage_type:Common.ListStorageType.Bytes1 ~codecs:uint8_list_codecs
      num_elements pointer_bytes

  let init_uint16_list
      (num_elements : int)
      (pointer_bytes : rw Slice.t)
    : (rw, int, rw ListStorage.t) InnerArray.t =
    init_list ~storage_type:Common.ListStorageType.Bytes2 ~codecs:uint16_list_codecs
      num_elements pointer_bytes

  let init_uint32_list
      (num_elements : int)
      (pointer_bytes : rw Slice.t)
    : (rw, Uint32.t, rw ListStorage.t) InnerArray.t =
    init_list ~storage_type:Common.ListStorageType.Bytes4 ~codecs:uint32_list_codecs
      num_elements pointer_bytes

  let init_uint64_list
      (num_elements : int)
      (pointer_bytes : rw Slice.t)
    : (rw, Uint64.t, rw ListStorage.t) InnerArray.t =
    init_list ~storage_type:Common.ListStorageType.Bytes8 ~codecs:uint64_list_codecs
      num_elements pointer_bytes

  let init_float32_list
      (num_elements : int)
      (pointer_bytes : rw Slice.t)
    : (rw, float, rw ListStorage.t) InnerArray.t =
    init_list ~storage_type:Common.ListStorageType.Bytes4 ~codecs:float32_list_codecs
      num_elements pointer_bytes

  let init_float64_list
      (num_elements : int)
      (pointer_bytes : rw Slice.t)
    : (rw, float, rw ListStorage.t) InnerArray.t =
    init_list ~storage_type:Common.ListStorageType.Bytes8 ~codecs:float64_list_codecs
      num_elements pointer_bytes

  let init_text_list
      (num_elements : int)
      (pointer_bytes : rw Slice.t)
    : (rw, string, rw ListStorage.t) InnerArray.t =
    init_list ~storage_type:Common.ListStorageType.Pointer ~codecs:text_list_codecs
      num_elements pointer_bytes

  let init_blob_list
      (num_elements : int)
      (pointer_bytes : rw Slice.t)
    : (rw, string, rw ListStorage.t) InnerArray.t =
    init_list ~storage_type:Common.ListStorageType.Pointer ~codecs:blob_list_codecs
      num_elements pointer_bytes

  let init_struct_list
      ~(data_words : int)
      ~(pointer_words : int)
      (num_elements : int)
      (pointer_bytes : rw Slice.t)
    : (rw, rw StructStorage.t, rw ListStorage.t) InnerArray.t =
    init_list ~storage_type:(
      Common.ListStorageType.Composite (data_words, pointer_words))
      ~codecs:struct_list_codecs num_elements pointer_bytes

  let init_struct
      ~(data_words : int)
      ~(pointer_words : int)
      (pointer_bytes : rw Slice.t)
    : rw StructStorage.t =
    let () = BOps.deep_zero_pointer pointer_bytes in
    let storage =
      BOps.alloc_struct_storage pointer_bytes.Slice.msg ~data_words ~pointer_words
    in
    let () = BOps.init_struct_pointer pointer_bytes storage in
    storage

  (* Locate the storage region corresponding to the root struct of a message.
     The [data_words] and [pointer_words] specify the expected struct layout. *)
  let get_root_struct
      (m : rw Message.t)
      ~(data_words : int)
      ~(pointer_words : int)
    : rw StructStorage.t =
    let first_segment = Message.get_segment m 0 in
    if Segment.length first_segment < sizeof_uint64 then
      invalid_msg "message is too small to contain root struct pointer"
    else
      let pointer_bytes = {
        Slice.msg        = m;
        Slice.segment_id = 0;
        Slice.start      = 0;
        Slice.len        = sizeof_uint64
      } in
      let create_default message =
        BOps.alloc_struct_storage message ~data_words ~pointer_words
      in
      BOps.deref_struct_pointer ~create_default ~data_words ~pointer_words
        pointer_bytes


  (* Allocate a new message of at least the given [message_size], creating a
     root struct with the specified struct layout.
     Returns: newly-allocated root struct storage *)
  let alloc_root_struct
      ?(message_size : int option)
      ~(data_words : int)
      ~(pointer_words : int)
      ()
    : rw StructStorage.t =
    let act_message_size =
      let requested_size =
        match message_size with
        | Some x -> x
        | None   -> 8192
      in
      max requested_size ((data_words + pointer_words + 1) * sizeof_uint64)
    in
    let message = Message.create act_message_size in
    (* Has the important side effect of reserving space in the message for
       the root struct pointer... *)
    let _ = Slice.alloc message sizeof_uint64 in
    get_root_struct message ~data_words ~pointer_words

end


