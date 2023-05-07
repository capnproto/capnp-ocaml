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

module Uint32 = Stdint.Uint32
module Uint64 = Stdint.Uint64

type ro = Message.ro
type rw = Message.rw
let invalid_msg = Message.invalid_msg

let sizeof_uint64 = 8

(* Functor parameter: NM == "native message" *)
module Make (NM : RPC.S) = struct
  module RA_ = struct
    include ReaderInc.Make(NM)
  end

  module BA_ = struct
    module NM = NM

    (* DM == "defaults message", meaning "the type of messages that store default values" *)
    module DM = Message.BytesMessage

    module NC = struct
      include CommonInc.Make(NM)
    end

    (* DefaultsCopier will provide algorithms for making deep copies of default
       data from DM storage into native storage *)
    module DefaultsCopier = BuilderOps.Make(DM)(NM)

    (* Most of the Builder operations need to copy from native storage back into
       native storage *)
    module BOps = BuilderOps.Make(NM)(NM)

    (* Given a string, generate an orphaned cap'n proto List<Uint8> which contains
       the string content. *)
    let uint8_list_of_string
        ~(null_terminated : bool)   (* true if the data is expected to end in 0 *)
        ~(dest_message : rw NM.Message.t)
        (src : string)
      : rw NM.ListStorage.t =
      let list_storage = BOps.alloc_list_storage dest_message
          ListStorageType.Bytes1
          (String.length src + (if null_terminated then 1 else 0))
      in
      NM.Slice.blit_from_string
        ~src ~src_pos:0
        ~dst:list_storage.NM.ListStorage.storage ~dst_pos:0
        ~len:(String.length src);
      list_storage


    let void_list_codecs = NC.ListCodecs.Empty (
        (fun (x : unit) -> x), (fun (x : unit) -> x))

    let bit_list_codecs = NC.ListCodecs.Bit (
        (fun (x : bool) -> x), (fun (x : bool) -> x))

    let int8_list_codecs = NC.ListCodecs.Bytes1 (
        (fun slice -> NM.Slice.get_int8 slice 0),
          (fun v slice -> NM.Slice.set_int8 slice 0 v))

    let int16_list_codecs = NC.ListCodecs.Bytes2 (
        (fun slice -> NM.Slice.get_int16 slice 0),
          (fun v slice -> NM.Slice.set_int16 slice 0 v))

    let int32_list_codecs = NC.ListCodecs.Bytes4 (
        (fun slice -> NM.Slice.get_int32 slice 0),
          (fun v slice -> NM.Slice.set_int32 slice 0 v))

    let int64_list_codecs = NC.ListCodecs.Bytes8 (
        (fun slice -> NM.Slice.get_int64 slice 0),
          (fun v slice -> NM.Slice.set_int64 slice 0 v))

    let uint8_list_codecs = NC.ListCodecs.Bytes1 (
        (fun slice -> NM.Slice.get_uint8 slice 0),
          (fun v slice -> NM.Slice.set_uint8 slice 0 v))

    let uint16_list_codecs = NC.ListCodecs.Bytes2 (
        (fun slice -> NM.Slice.get_uint16 slice 0),
          (fun v slice -> NM.Slice.set_uint16 slice 0 v))

    let uint32_list_codecs = NC.ListCodecs.Bytes4 (
        (fun slice -> NM.Slice.get_uint32 slice 0),
          (fun v slice -> NM.Slice.set_uint32 slice 0 v))

    let uint64_list_codecs = NC.ListCodecs.Bytes8 (
        (fun slice -> NM.Slice.get_uint64 slice 0),
          (fun v slice -> NM.Slice.set_uint64 slice 0 v))

    let float32_list_codecs = NC.ListCodecs.Bytes4 (
        (fun slice -> Int32.float_of_bits (NM.Slice.get_int32 slice 0)),
          (fun v slice -> NM.Slice.set_int32 slice 0
            (Int32.bits_of_float v)))

    let float64_list_codecs = NC.ListCodecs.Bytes8 (
        (fun slice -> Int64.float_of_bits (NM.Slice.get_int64 slice 0)),
          (fun v slice -> NM.Slice.set_int64 slice 0
            (Int64.bits_of_float v)))

    let text_list_codecs =
      let decode slice =
        (* Text fields are always accessed by value, not by reference, since
           we always do an immediate decode to [string].  Therefore we can
           use the Reader logic to handle this case. *)
        match RA_.deref_list_pointer slice with
        | Some list_storage ->
            NC.string_of_uint8_list ~null_terminated:true list_storage
        | None ->
            ""
      in
      let encode s slice =
        let new_list_storage = uint8_list_of_string ~null_terminated:true
            ~dest_message:slice.NM.Slice.msg s
        in
        BOps.init_list_pointer slice new_list_storage
      in
      NC.ListCodecs.Pointer (decode, encode)

    let blob_list_codecs =
      let decode slice =
        (* Data fields are always accessed by value, not by reference, since
           we always do an immediate decode to [string].  Therefore we can
           use the Reader logic to handle this case. *)
        match RA_.deref_list_pointer slice with
        | Some list_storage ->
            NC.string_of_uint8_list ~null_terminated:false list_storage
        | None ->
            ""
      in
      let encode s slice =
        let new_list_storage = uint8_list_of_string ~null_terminated:false
            ~dest_message:slice.NM.Slice.msg s
        in
        BOps.init_list_pointer slice new_list_storage
      in
      NC.ListCodecs.Pointer (decode, encode)

    let struct_list_codecs =
      let bytes_decoder slice =
        NC.struct_of_bytes_slice slice
      in
      let bytes_encoder v slice =
        let dest = NC.struct_of_bytes_slice slice in
        BOps.deep_copy_struct_to_dest ~src:v ~dest
      in
      let pointer_decoder slice =
        NC.struct_of_pointer_slice slice
      in
      let pointer_encoder v slice =
        let dest = NC.struct_of_pointer_slice slice in
        BOps.deep_copy_struct_to_dest ~src:v ~dest
      in
      let composite_decoder x = NC.StructStorage.cast x in
      let composite_encoder v dest = BOps.deep_copy_struct_to_dest ~src:v ~dest in
      NC.ListCodecs.Struct {
        NC.ListCodecs.bytes     = (bytes_decoder, bytes_encoder);
        NC.ListCodecs.pointer   = (pointer_decoder, pointer_encoder);
        NC.ListCodecs.composite = (composite_decoder, composite_encoder);
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
        (data : rw NM.Slice.t)
        (discr : Discr.t option)
      : unit =
      match discr with
      | None ->
          ()
      | Some x ->
          set_uint16 data ~default:0 ~byte_ofs:x.Discr.byte_ofs x.Discr.value

    and set_uint16
        ?(discr : Discr.t option)
        (data : rw NM.Slice.t)
        ~(default : int)
        ~(byte_ofs : int)
        (value : int)
      : unit =
      let () = set_opt_discriminant data discr in
      NM.Slice.set_uint16 data byte_ofs (value lxor default)


    (* Given storage for a struct, get the bytes associated with the
       struct data section.  If the optional discriminant parameter is
       supplied, then the discriminant is also set as a side-effect. *)
    let get_data_region
        ?(discr : Discr.t option)
        (struct_storage : (rw, _) NM.StructStorage.t)
      : rw NM.Slice.t =
      let data = struct_storage.NM.StructStorage.data in
      let () = set_opt_discriminant data discr in
      data

    let get_bit
       ~(default : bool)
       (struct_storage : (rw, _) NM.StructStorage.t)
       ~(byte_ofs : int)
       ~(bit_ofs : int)
      : bool =
      let data = struct_storage.NM.StructStorage.data in
      let byte_val = NM.Slice.get_uint8 data byte_ofs in
      let is_set = Util.get_bit byte_val bit_ofs in
      if default then
        not is_set
      else
        is_set

    let get_int8
        ~(default : int)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (byte_ofs : int)
      : int =
      let data = struct_storage.NM.StructStorage.data in
      let numeric = NM.Slice.get_int8 data byte_ofs in
      numeric lxor default

    let get_int16
        ~(default : int)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (byte_ofs : int)
      : int =
      let data = struct_storage.NM.StructStorage.data in
      let numeric = NM.Slice.get_int16 data byte_ofs in
      numeric lxor default

    let get_int32
        ~(default : int32)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (byte_ofs : int)
        : int32 =
      let data = struct_storage.NM.StructStorage.data in
      let numeric = NM.Slice.get_int32 data byte_ofs in
      Int32.logxor numeric default

    let get_int64
        ~(default : int64)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (byte_ofs : int)
      : int64 =
      let data = struct_storage.NM.StructStorage.data in
      let numeric = NM.Slice.get_int64 data byte_ofs in
      Int64.logxor numeric default

    let get_uint8
        ~(default : int)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (byte_ofs : int)
      : int =
      let data = struct_storage.NM.StructStorage.data in
      let numeric = NM.Slice.get_uint8 data byte_ofs in
      numeric lxor default

    let get_uint16
        ~(default : int)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (byte_ofs : int)
      : int =
      let data = struct_storage.NM.StructStorage.data in
      let numeric = NM.Slice.get_uint16 data byte_ofs in
      numeric lxor default

    let get_uint32
        ~(default : Uint32.t)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (byte_ofs : int)
      : Uint32.t =
      let data = struct_storage.NM.StructStorage.data in
      let numeric = NM.Slice.get_uint32 data byte_ofs in
      Uint32.logxor numeric default

    let get_uint64
        ~(default : Uint64.t)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (byte_ofs : int)
      : Uint64.t =
      let data = struct_storage.NM.StructStorage.data in
      let numeric = NM.Slice.get_uint64 data byte_ofs in
      Uint64.logxor numeric default

    let get_float32
        ~(default_bits : int32)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (byte_ofs : int)
      : float =
      let data = struct_storage.NM.StructStorage.data in
      let numeric = NM.Slice.get_int32 data byte_ofs in
      let bits = Int32.logxor numeric default_bits in
      Int32.float_of_bits bits

    let get_float64
        ~(default_bits : int64)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (byte_ofs : int)
      : float =
      let data = struct_storage.NM.StructStorage.data in
      let numeric = NM.Slice.get_int64 data byte_ofs in
      let bits = Int64.logxor numeric default_bits in
      Int64.float_of_bits bits


    (*******************************************************************************
     * METHODS FOR SETTING OBJECTS STORED BY VALUE
     *******************************************************************************)

    let set_void
        ?(discr : Discr.t option)
        (struct_storage : (rw, _) NM.StructStorage.t)
      : unit =
      let data = struct_storage.NM.StructStorage.data in
      set_opt_discriminant data discr

    let set_bit
        ?(discr : Discr.t option)
        ~(default : bool)
        (struct_storage : (rw, _) NM.StructStorage.t)
        ~(byte_ofs : int)
        ~(bit_ofs : int)
        (value : bool)
      : unit =
      let data = struct_storage.NM.StructStorage.data in
      let () = set_opt_discriminant data discr in
      let default_bit = Util.int_of_bool default in
      let value_bit = Util.int_of_bool value in
      let stored_bit = default_bit lxor value_bit in
      let byte_val = NM.Slice.get_uint8 data byte_ofs in
      let byte_val = byte_val land (lnot (1 lsl bit_ofs)) in
      let byte_val = byte_val lor (stored_bit lsl bit_ofs) in
      NM.Slice.set_uint8 data byte_ofs byte_val

    let set_int8
        ?(discr : Discr.t option)
        ~(default : int)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (byte_ofs : int)
        (value : int)
      : unit =
      let data = struct_storage.NM.StructStorage.data in
      let () = set_opt_discriminant data discr in
      NM.Slice.set_int8 data byte_ofs (value lxor default)

    let set_int16
        ?(discr : Discr.t option)
        ~(default : int)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (byte_ofs : int)
        (value : int)
      : unit =
      let data = struct_storage.NM.StructStorage.data in
      let () = set_opt_discriminant data discr in
      NM.Slice.set_int16 data byte_ofs (value lxor default)

    let set_int32
        ?(discr : Discr.t option)
        ~(default : int32)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (byte_ofs : int)
        (value : int32)
      : unit =
      let data = struct_storage.NM.StructStorage.data in
      let () = set_opt_discriminant data discr in
      NM.Slice.set_int32 data byte_ofs (Int32.logxor value default)

    let set_int64
        ?(discr : Discr.t option)
        ~(default : int64)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (byte_ofs : int)
        (value : int64)
      : unit =
      let data = struct_storage.NM.StructStorage.data in
      let () = set_opt_discriminant data discr in
      NM.Slice.set_int64 data byte_ofs (Int64.logxor value default)

    let set_uint8
        ?(discr : Discr.t option)
        ~(default : int)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (byte_ofs : int)
        (value : int)
      : unit =
      let data = struct_storage.NM.StructStorage.data in
      let () = set_opt_discriminant data discr in
      NM.Slice.set_uint8 data byte_ofs (value lxor default)

    let set_uint16
        ?(discr : Discr.t option)
        ~(default : int)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (byte_ofs : int)
        (value : int)
      : unit =
      let data = struct_storage.NM.StructStorage.data in
      let () = set_opt_discriminant data discr in
      NM.Slice.set_uint16 data byte_ofs (value lxor default)

    let set_uint32
        ?(discr : Discr.t option)
        ~(default : Uint32.t)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (byte_ofs : int)
        (value : Uint32.t)
      : unit =
      let data = struct_storage.NM.StructStorage.data in
      let () = set_opt_discriminant data discr in
      NM.Slice.set_uint32 data byte_ofs (Uint32.logxor value default)

    let set_uint64
        ?(discr : Discr.t option)
        ~(default : Uint64.t)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (byte_ofs : int)
        (value : Uint64.t)
      : unit =
      let data = struct_storage.NM.StructStorage.data in
      let () = set_opt_discriminant data discr in
      NM.Slice.set_uint64 data byte_ofs (Uint64.logxor value default)

    let set_float32
        ?(discr : Discr.t option)
        ~(default_bits : int32)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (byte_ofs : int)
        (value : float)
      : unit =
      let data = struct_storage.NM.StructStorage.data in
      let () = set_opt_discriminant data discr in
      NM.Slice.set_int32 data byte_ofs
        (Int32.logxor (Int32.bits_of_float value) default_bits)

    let set_float64
        ?(discr : Discr.t option)
        ~(default_bits : int64)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (byte_ofs : int)
        (value : float)
      : unit =
      let data = struct_storage.NM.StructStorage.data in
      let () = set_opt_discriminant data discr in
      NM.Slice.set_int64 data byte_ofs
        (Int64.logxor (Int64.bits_of_float value) default_bits)


    (*******************************************************************************
     * METHODS FOR GETTING OBJECTS STORED BY POINTER
     *******************************************************************************)

    let has_field
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
      : bool =
      let pointers = struct_storage.NM.StructStorage.pointers in
      let num_pointers = pointers.NM.Slice.len / sizeof_uint64 in
      (* Struct should have already been upgraded to at least the
         expected data region and pointer region sizes *)
      assert (pointer_word < num_pointers);
      let pointer64 = NM.Slice.get_int64 pointers (pointer_word * sizeof_uint64) in
      not (Util.is_int64_zero pointer64)

    let get_text
        ~(default : string)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
      : string =
      let pointers = struct_storage.NM.StructStorage.pointers in
      let num_pointers = pointers.NM.Slice.len / sizeof_uint64 in
      (* Struct should have already been upgraded to at least the
         expected data region and pointer region sizes *)
      assert (pointer_word < num_pointers);
      let pointer_bytes = {
        pointers with
        NM.Slice.start = pointers.NM.Slice.start + (pointer_word * sizeof_uint64);
        NM.Slice.len   = sizeof_uint64;
      } in
      (* Text fields are always accessed by value, not by reference, since
         we always do an immediate decode to [string].  Therefore we can
         use the Reader logic to handle this case. *)
      match RA_.deref_list_pointer pointer_bytes with
      | Some list_storage ->
          NC.string_of_uint8_list ~null_terminated:true list_storage
      | None ->
          default

    let get_blob
        ~(default : string)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
      : string =
      let pointers = struct_storage.NM.StructStorage.pointers in
      let num_pointers = pointers.NM.Slice.len / sizeof_uint64 in
      (* Struct should have already been upgraded to at least the
         expected data region and pointer region sizes *)
      assert (pointer_word < num_pointers);
      let pointer_bytes = {
        pointers with
        NM.Slice.start = pointers.NM.Slice.start + (pointer_word * sizeof_uint64);
        NM.Slice.len   = sizeof_uint64;
      } in
      (* Data fields are always accessed by value, not by reference, since
         we always do an immediate decode to [string].  Therefore we can
         use the Reader logic to handle this case. *)
      match RA_.deref_list_pointer pointer_bytes with
      | Some list_storage ->
          NC.string_of_uint8_list ~null_terminated:false list_storage
      | None ->
          default


    (* Zero-initialize list storage of the given length and storage type, 
       associating it with the specified list pointer. *)
    let init_list_storage
        ~(storage_type : ListStorageType.t)
        ~(num_elements : int)
        (pointer_bytes : rw NM.Slice.t)
      : rw NM.ListStorage.t =
      let () = BOps.deep_zero_pointer pointer_bytes in
      let message = pointer_bytes.NM.Slice.msg in
      let list_storage = BOps.alloc_list_storage message storage_type num_elements in
      let () = BOps.init_list_pointer pointer_bytes list_storage in
      list_storage


    let get_list
        ?(struct_sizes : BuilderOps.StructSizes.t option)
        ?(default : ro DM.ListStorage.t option)
        ~(storage_type : ListStorageType.t)
        ~(codecs : 'a NC.ListCodecs.t)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
      : (rw, 'a, rw NM.ListStorage.t) InnerArray.t =
      let create_default message =
        match default with
        | Some default_storage ->
            DefaultsCopier.deep_copy_list ?struct_sizes
              ~src:default_storage ~dest_message:message ()
        | None ->
            BOps.alloc_list_storage message storage_type 0
      in
      let pointers = struct_storage.NM.StructStorage.pointers in
      let num_pointers = pointers.NM.Slice.len / sizeof_uint64 in
      (* Struct should have already been upgraded to at least the
         expected data region and pointer region sizes *)
      assert (pointer_word < num_pointers);
      let pointer_bytes = {
        pointers with
        NM.Slice.start = pointers.NM.Slice.start + (pointer_word * sizeof_uint64);
        NM.Slice.len   = sizeof_uint64;
      } in
      let list_storage = BOps.deref_list_pointer ?struct_sizes ~create_default
          pointer_bytes
      in
      NC.make_array_readwrite ~list_storage ~codecs
        ~init:(fun n -> init_list_storage ~storage_type ~num_elements:n pointer_bytes)

    let get_void_list
        ?(default : ro DM.ListStorage.t option)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
      : (rw, unit, rw NM.ListStorage.t) InnerArray.t =
      get_list ?default ~storage_type:ListStorageType.Empty
        ~codecs:void_list_codecs struct_storage pointer_word

    let get_bit_list
        ?(default : ro DM.ListStorage.t option)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
      : (rw, bool, rw NM.ListStorage.t) InnerArray.t =
      get_list ?default ~storage_type:ListStorageType.Bit
        ~codecs:bit_list_codecs struct_storage pointer_word

    let get_int8_list
        ?(default : ro DM.ListStorage.t option)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
      : (rw, int, rw NM.ListStorage.t) InnerArray.t =
      get_list ?default ~storage_type:ListStorageType.Bytes1
        ~codecs:int8_list_codecs struct_storage pointer_word

    let get_int16_list
        ?(default : ro DM.ListStorage.t option)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
      : (rw, int, rw NM.ListStorage.t) InnerArray.t =
      get_list ?default ~storage_type:ListStorageType.Bytes2
        ~codecs:int16_list_codecs struct_storage pointer_word

    let get_int32_list
        ?(default : ro DM.ListStorage.t option)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
      : (rw, int32, rw NM.ListStorage.t) InnerArray.t =
      get_list ?default ~storage_type:ListStorageType.Bytes4
        ~codecs:int32_list_codecs struct_storage pointer_word

    let get_int64_list
        ?(default : ro DM.ListStorage.t option)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
      : (rw, int64, rw NM.ListStorage.t) InnerArray.t =
      get_list ?default ~storage_type:ListStorageType.Bytes8
        ~codecs:int64_list_codecs struct_storage pointer_word

    let get_uint8_list
        ?(default : ro DM.ListStorage.t option)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
      : (rw, int, rw NM.ListStorage.t) InnerArray.t =
      get_list ?default ~storage_type:ListStorageType.Bytes1
        ~codecs:uint8_list_codecs struct_storage pointer_word

    let get_uint16_list
        ?(default : ro DM.ListStorage.t option)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
      : (rw, int, rw NM.ListStorage.t) InnerArray.t =
      get_list ?default ~storage_type:ListStorageType.Bytes2
        ~codecs:uint16_list_codecs struct_storage pointer_word

    let get_uint32_list
        ?(default : ro DM.ListStorage.t option)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
      : (rw, Uint32.t, rw NM.ListStorage.t) InnerArray.t =
      get_list ?default ~storage_type:ListStorageType.Bytes4
        ~codecs:uint32_list_codecs struct_storage pointer_word

    let get_uint64_list
        ?(default : ro DM.ListStorage.t option)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
      : (rw, Uint64.t, rw NM.ListStorage.t) InnerArray.t =
      get_list ?default ~storage_type:ListStorageType.Bytes8
        ~codecs:uint64_list_codecs struct_storage pointer_word

    let get_float32_list
        ?(default : ro DM.ListStorage.t option)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
      : (rw, float, rw NM.ListStorage.t) InnerArray.t =
      get_list ?default ~storage_type:ListStorageType.Bytes4
        ~codecs:float32_list_codecs struct_storage pointer_word

    let get_float64_list
        ?(default : ro DM.ListStorage.t option)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
      : (rw, float, rw NM.ListStorage.t) InnerArray.t =
      get_list ?default ~storage_type:ListStorageType.Bytes8
        ~codecs:float64_list_codecs struct_storage pointer_word

    let get_text_list
        ?(default : ro DM.ListStorage.t option)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
      : (rw, string, rw NM.ListStorage.t) InnerArray.t =
      get_list ?default ~storage_type:ListStorageType.Pointer
        ~codecs:text_list_codecs struct_storage pointer_word

    let get_blob_list
        ?(default : ro DM.ListStorage.t option)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
      : (rw, string, rw NM.ListStorage.t) InnerArray.t =
      get_list ?default ~storage_type:ListStorageType.Pointer
        ~codecs:blob_list_codecs struct_storage pointer_word

    let get_struct_list
        ?(default : ro DM.ListStorage.t option)
        ~(data_words : int)
        ~(pointer_words : int)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
      : (rw, (rw, _) NM.StructStorage.t, rw NM.ListStorage.t) InnerArray.t =
      get_list ~struct_sizes:{
        BuilderOps.StructSizes.data_words;
        BuilderOps.StructSizes.pointer_words }
        ?default ~storage_type:(
          ListStorageType.Composite (data_words, pointer_words))
        ~codecs:struct_list_codecs struct_storage pointer_word

    let get_struct
        ?(default : (ro, 'a) DM.StructStorage.t option)
        ~(data_words : int)
        ~(pointer_words : int)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
      : (rw, 'a) NM.StructStorage.t =
      let create_default message =
        match default with
        | Some default_storage ->
            DefaultsCopier.deep_copy_struct ~src:default_storage ~dest_message:message
              ~data_words ~pointer_words
        | None ->
            BOps.alloc_struct_storage message ~data_words ~pointer_words
      in
      let pointers = struct_storage.NM.StructStorage.pointers in
      let num_pointers = pointers.NM.Slice.len / sizeof_uint64 in
      (* Struct should have already been upgraded to at least the
         expected data region and pointer region sizes *)
      assert (pointer_word < num_pointers);
      let pointer_bytes = {
        pointers with
        NM.Slice.start = pointers.NM.Slice.start + (pointer_word * sizeof_uint64);
        NM.Slice.len   = sizeof_uint64;
      } in
      BOps.deref_struct_pointer ~create_default ~data_words ~pointer_words pointer_bytes

    let get_pointer
        ?(default : ro DM.Slice.t option)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
      : rw NM.Slice.t =
      let pointers = struct_storage.NM.StructStorage.pointers in
      let num_pointers = pointers.NM.Slice.len / sizeof_uint64 in
      (* Struct should have already been upgraded to at least the
         expected data region and pointer region sizes *)
      assert (pointer_word < num_pointers);
      let pointer_bytes = {
        pointers with
        NM.Slice.start = pointers.NM.Slice.start + (pointer_word * sizeof_uint64);
        NM.Slice.len   = sizeof_uint64;
      } in
      let () =
        let pointer_val = NM.Slice.get_int64 pointer_bytes 0 in
        if Util.is_int64_zero pointer_val then
          match default with
          | Some default_pointer ->
              DefaultsCopier.deep_copy_pointer ~src:default_pointer
                ~dest:pointer_bytes
          | None ->
              ()
        else
          ()
      in
      pointer_bytes

    let get_interface
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
      : 'a option =
      let pointers = struct_storage.NM.StructStorage.pointers in
      let num_pointers = pointers.NM.Slice.len / sizeof_uint64 in
      (* Struct should have already been upgraded to at least the
         expected data region and pointer region sizes *)
      assert (pointer_word < num_pointers);
      let pointer_bytes = {
        pointers with
        NM.Slice.start = pointers.NM.Slice.start + (pointer_word * sizeof_uint64);
        NM.Slice.len   = sizeof_uint64;
      } in
      match NC.decode_pointer pointer_bytes with
      | Pointer.Null ->
          None
      | Pointer.Other (OtherPointer.Capability index) ->
          let attachments = NM.Message.get_attachments pointers.NM.Slice.msg in
          Some (NM.Untyped.get_cap attachments index)
      | _ ->
          invalid_msg "decoded non-capability pointer where capability was expected"


    (*******************************************************************************
     * METHODS FOR SETTING OBJECTS STORED BY POINTER
     *******************************************************************************)

    let set_text
        ?(discr : Discr.t option)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
        (value : string)
      : unit =
      let pointers = struct_storage.NM.StructStorage.pointers in
      let num_pointers = pointers.NM.Slice.len / sizeof_uint64 in
      (* Struct should have already been upgraded to at least the
         expected data region and pointer region sizes *)
      assert (pointer_word < num_pointers);
      let pointer_bytes = {
        pointers with
        NM.Slice.start = pointers.NM.Slice.start + (pointer_word * sizeof_uint64);
        NM.Slice.len   = sizeof_uint64;
      } in
      let () = set_opt_discriminant struct_storage.NM.StructStorage.data discr in
      let new_string_storage = uint8_list_of_string
        ~null_terminated:true ~dest_message:pointer_bytes.NM.Slice.msg
        value
      in
      let () = BOps.deep_zero_pointer pointer_bytes in
      BOps.init_list_pointer pointer_bytes new_string_storage

    let set_blob
        ?(discr : Discr.t option)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
        (value : string)
      : unit =
      let pointers = struct_storage.NM.StructStorage.pointers in
      let num_pointers = pointers.NM.Slice.len / sizeof_uint64 in
      (* Struct should have already been upgraded to at least the
         expected data region and pointer region sizes *)
      assert (pointer_word < num_pointers);
      let pointer_bytes = {
        pointers with
        NM.Slice.start = pointers.NM.Slice.start + (pointer_word * sizeof_uint64);
        NM.Slice.len   = sizeof_uint64;
      } in
      let () = set_opt_discriminant struct_storage.NM.StructStorage.data discr in
      let new_string_storage = uint8_list_of_string
        ~null_terminated:false ~dest_message:pointer_bytes.NM.Slice.msg
        value
      in
      let () = BOps.deep_zero_pointer pointer_bytes in
      BOps.init_list_pointer pointer_bytes new_string_storage

    let set_list_from_storage
        ?(struct_sizes : BuilderOps.StructSizes.t option)
        ~(storage_type : ListStorageType.t)
        ~(codecs : 'a NC.ListCodecs.t)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
        (value : 'cap NM.ListStorage.t option)
      : (rw, 'a, rw NM.ListStorage.t) InnerArray.t =
      let pointers = struct_storage.NM.StructStorage.pointers in
      let num_pointers = pointers.NM.Slice.len / sizeof_uint64 in
      (* Struct should have already been upgraded to at least the
         expected data region and pointer region sizes *)
      assert (pointer_word < num_pointers);
      let pointer_bytes = {
        pointers with
        NM.Slice.start = pointers.NM.Slice.start + (pointer_word * sizeof_uint64);
        NM.Slice.len   = sizeof_uint64;
      } in
      let list_storage =
        match value with
        | Some src_storage ->
            BOps.deep_copy_list ?struct_sizes
              ~src:src_storage ~dest_message:pointer_bytes.NM.Slice.msg ()
        | None ->
            BOps.alloc_list_storage pointer_bytes.NM.Slice.msg storage_type 0
      in
      let () = BOps.deep_zero_pointer pointer_bytes in
      let () = BOps.init_list_pointer pointer_bytes list_storage in
      NC.make_array_readwrite ~list_storage ~codecs
        ~init:(fun n -> init_list_storage ~storage_type ~num_elements:n pointer_bytes)

    let set_list
        ?(discr : Discr.t option)
        ?(struct_sizes : BuilderOps.StructSizes.t option)
        ~(storage_type : ListStorageType.t)
        ~(codecs : 'a NC.ListCodecs.t)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
        (value : ('cap1, 'a, 'cap2 NM.ListStorage.t) InnerArray.t)
      : (rw, 'a, rw NM.ListStorage.t) InnerArray.t =
      let () = set_opt_discriminant struct_storage.NM.StructStorage.data discr in
      set_list_from_storage ?struct_sizes ~storage_type ~codecs
        struct_storage pointer_word (InnerArray.to_storage value)

    let set_void_list
        ?(discr : Discr.t option)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
        (value : ('cap1, unit, 'cap2 NM.ListStorage.t) InnerArray.t)
      : (rw, unit, rw NM.ListStorage.t) InnerArray.t =
      set_list ?discr ~storage_type:ListStorageType.Empty ~codecs:void_list_codecs
        struct_storage pointer_word value

    let set_bit_list
        ?(discr : Discr.t option)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
        (value : ('cap1, bool, 'cap2 NM.ListStorage.t) InnerArray.t)
      : (rw, bool, rw NM.ListStorage.t) InnerArray.t =
      set_list ?discr ~storage_type:ListStorageType.Bit ~codecs:bit_list_codecs
        struct_storage pointer_word value

    let set_int8_list
        ?(discr : Discr.t option)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
        (value : ('cap1, int, 'cap2 NM.ListStorage.t) InnerArray.t)
      : (rw, int, 'cap NM.ListStorage.t) InnerArray.t =
      set_list ?discr ~storage_type:ListStorageType.Bytes1 ~codecs:int8_list_codecs
        struct_storage pointer_word value

    let set_int16_list
        ?(discr : Discr.t option)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
        (value : ('cap1, int, 'cap2 NM.ListStorage.t) InnerArray.t)
      : (rw, int, 'cap NM.ListStorage.t) InnerArray.t =
      set_list ?discr ~storage_type:ListStorageType.Bytes2 ~codecs:int16_list_codecs
        struct_storage pointer_word value

    let set_int32_list
        ?(discr : Discr.t option)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
        (value : ('cap1, int32, 'cap NM.ListStorage.t) InnerArray.t)
      : (rw, int32, rw NM.ListStorage.t) InnerArray.t =
      set_list ?discr ~storage_type:ListStorageType.Bytes4 ~codecs:int32_list_codecs
        struct_storage pointer_word value

    let set_int64_list
        ?(discr : Discr.t option)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
        (value : ('cap1, int64, 'cap2 NM.ListStorage.t) InnerArray.t)
      : (rw, int64, rw NM.ListStorage.t) InnerArray.t =
      set_list ?discr ~storage_type:ListStorageType.Bytes8 ~codecs:int64_list_codecs
        struct_storage pointer_word value

    let set_uint8_list
        ?(discr : Discr.t option)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
        (value : ('cap1, int, 'cap2 NM.ListStorage.t) InnerArray.t)
      : (rw, int, rw NM.ListStorage.t) InnerArray.t =
      set_list ?discr ~storage_type:ListStorageType.Bytes1 ~codecs:uint8_list_codecs
        struct_storage pointer_word value

    let set_uint16_list
        ?(discr : Discr.t option)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
        (value : ('cap1, int, 'cap2 NM.ListStorage.t) InnerArray.t)
      : (rw, int, rw NM.ListStorage.t) InnerArray.t =
      set_list ?discr ~storage_type:ListStorageType.Bytes2 ~codecs:uint16_list_codecs
        struct_storage pointer_word value

    let set_uint32_list
        ?(discr : Discr.t option)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
        (value : ('cap1, Uint32.t, 'cap2 NM.ListStorage.t) InnerArray.t)
      : (rw, Uint32.t, rw NM.ListStorage.t) InnerArray.t =
      set_list ?discr ~storage_type:ListStorageType.Bytes4 ~codecs:uint32_list_codecs
        struct_storage pointer_word value

    let set_uint64_list
        ?(discr : Discr.t option)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
        (value : ('cap1, Uint64.t, 'cap2 NM.ListStorage.t) InnerArray.t)
      : (rw, Uint64.t, rw NM.ListStorage.t) InnerArray.t =
      set_list ?discr ~storage_type:ListStorageType.Bytes8 ~codecs:uint64_list_codecs
        struct_storage pointer_word value

    let set_float32_list
        ?(discr : Discr.t option)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
        (value : ('cap1, float, 'cap2 NM.ListStorage.t) InnerArray.t)
      : (rw, float, rw NM.ListStorage.t) InnerArray.t =
      set_list ?discr ~storage_type:ListStorageType.Bytes4 ~codecs:float32_list_codecs
        struct_storage pointer_word value

    let set_float64_list
        ?(discr : Discr.t option)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
        (value : ('cap1, float, 'cap2 NM.ListStorage.t) InnerArray.t)
      : (rw, float, rw NM.ListStorage.t) InnerArray.t =
      set_list ?discr ~storage_type:ListStorageType.Bytes8 ~codecs:float64_list_codecs
        struct_storage pointer_word value

    let set_text_list
        ?(discr : Discr.t option)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
        (value : ('cap1, string, 'cap2 NM.ListStorage.t) InnerArray.t)
      : (rw, string, rw NM.ListStorage.t) InnerArray.t =
      set_list ?discr ~storage_type:ListStorageType.Pointer ~codecs:text_list_codecs
        struct_storage pointer_word value

    let set_blob_list
        ?(discr : Discr.t option)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
        (value : ('cap1, string, 'cap2 NM.ListStorage.t) InnerArray.t)
      : (rw, string, rw NM.ListStorage.t) InnerArray.t =
      set_list ?discr ~storage_type:ListStorageType.Pointer ~codecs:blob_list_codecs
        struct_storage pointer_word value

    let set_struct_list
        ?(discr : Discr.t option)
        ~(data_words : int)
        ~(pointer_words : int)
        (* FIXME: this won't allow assignment from Reader struct lists *)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
        (value : ('cap1, ('cap2, _) NM.StructStorage.t, 'cap2 NM.ListStorage.t) InnerArray.t)
      : (rw, (rw, _) NM.StructStorage.t, rw NM.ListStorage.t) InnerArray.t =
      set_list ?discr ~struct_sizes:{
        BuilderOps.StructSizes.data_words;
        BuilderOps.StructSizes.pointer_words }
        ~storage_type:(ListStorageType.Composite (data_words, pointer_words))
        ~codecs:struct_list_codecs struct_storage pointer_word value

    let set_struct
        ?(discr : Discr.t option)
        ~(data_words : int)
        ~(pointer_words : int)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
        (value : ('cap, 'a) NM.StructStorage.t option)
      : (rw, 'a) NM.StructStorage.t =
      let pointers = struct_storage.NM.StructStorage.pointers in
      let num_pointers = pointers.NM.Slice.len / sizeof_uint64 in
      (* Struct should have already been upgraded to at least the
         expected data region and pointer region sizes *)
      assert (pointer_word < num_pointers);
      let pointer_bytes = {
        pointers with
        NM.Slice.start = pointers.NM.Slice.start + (pointer_word * sizeof_uint64);
        NM.Slice.len   = sizeof_uint64;
      } in
      let () = set_opt_discriminant struct_storage.NM.StructStorage.data discr in
      let dest_storage =
        match value with
        | Some src_storage ->
            BOps.deep_copy_struct ~src:src_storage
              ~dest_message:pointer_bytes.NM.Slice.msg ~data_words ~pointer_words
        | None ->
            BOps.alloc_struct_storage pointer_bytes.NM.Slice.msg ~data_words ~pointer_words
      in
      let () = BOps.deep_zero_pointer pointer_bytes in
      let () = BOps.init_struct_pointer pointer_bytes dest_storage in
      dest_storage

    let set_pointer
        ?(discr : Discr.t option)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
        (value : 'cap NM.Slice.t option)
      : rw NM.Slice.t =
      let pointers = struct_storage.NM.StructStorage.pointers in
      let num_pointers = pointers.NM.Slice.len / sizeof_uint64 in
      (* Struct should have already been upgraded to at least the
         expected data region and pointer region sizes *)
      assert (pointer_word < num_pointers);
      let pointer_bytes = {
        pointers with
        NM.Slice.start = pointers.NM.Slice.start + (pointer_word * sizeof_uint64);
        NM.Slice.len   = sizeof_uint64;
      } in
      let () = set_opt_discriminant struct_storage.NM.StructStorage.data discr in
      let () = BOps.deep_zero_pointer pointer_bytes in
      let () =
        match value with
        | Some value -> BOps.deep_copy_pointer ~src:value ~dest:pointer_bytes
        | None -> ()
      in
      pointer_bytes

    let set_interface
        ?(discr : Discr.t option)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
        (value : 'a option)
      : unit =
      let pointers = struct_storage.NM.StructStorage.pointers in
      let num_pointers = pointers.NM.Slice.len / sizeof_uint64 in
      (* Struct should have already been upgraded to at least the
         expected data region and pointer region sizes *)
      assert (pointer_word < num_pointers);
      let pointer_bytes = {
        pointers with
        NM.Slice.start = pointers.NM.Slice.start + (pointer_word * sizeof_uint64);
        NM.Slice.len   = sizeof_uint64;
      } in
      let attachments = NM.Message.get_attachments pointers.NM.Slice.msg in
      set_opt_discriminant struct_storage.NM.StructStorage.data discr;
      BOps.deep_zero_pointer pointer_bytes;
      match value with
      | Some v ->
          let index = NM.Untyped.add_cap attachments v in
          NM.Slice.set_int64 pointer_bytes 0
            (OtherPointer.encode (OtherPointer.Capability index))
      | None ->
          NM.Slice.set_int64 pointer_bytes 0 Int64.zero


    (*******************************************************************************
     * METHODS FOR INITIALIZING OBJECTS STORED BY POINTER
     *******************************************************************************)

    let init_blob
        ?(discr : Discr.t option)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
        (num_elements : int)
      : unit =
      let s = String.make num_elements '\x00' in
      set_blob ?discr struct_storage pointer_word s

    let init_list
        ?(discr : Discr.t option)
        ~(storage_type : ListStorageType.t)
        ~(codecs : 'a NC.ListCodecs.t)
        (struct_storage : ('cap, _) NM.StructStorage.t)
        (pointer_word : int)
        (num_elements : int)
      : (rw, 'a, rw NM.ListStorage.t) InnerArray.t =
      let pointers = struct_storage.NM.StructStorage.pointers in
      let num_pointers = pointers.NM.Slice.len / sizeof_uint64 in
      (* Struct should have already been upgraded to at least the
         expected data region and pointer region sizes *)
      assert (pointer_word < num_pointers);
      let pointer_bytes = {
        pointers with
        NM.Slice.start = pointers.NM.Slice.start + (pointer_word * sizeof_uint64);
        NM.Slice.len   = sizeof_uint64;
      } in
      let () = set_opt_discriminant struct_storage.NM.StructStorage.data discr in
      let list_storage = init_list_storage ~storage_type ~num_elements pointer_bytes in
      NC.make_array_readwrite ~list_storage ~codecs
        ~init:(fun n -> init_list_storage ~storage_type ~num_elements:n pointer_bytes)

    let init_void_list
        ?(discr : Discr.t option)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
        (num_elements : int)
      : (rw, unit, rw NM.ListStorage.t) InnerArray.t =
      init_list ?discr ~storage_type:ListStorageType.Empty ~codecs:void_list_codecs
        struct_storage pointer_word num_elements

    let init_bit_list
        ?(discr : Discr.t option)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
        (num_elements : int)
      : (rw, bool, rw NM.ListStorage.t) InnerArray.t =
      init_list ?discr ~storage_type:ListStorageType.Bit ~codecs:bit_list_codecs
        struct_storage pointer_word num_elements

    let init_int8_list
        ?(discr : Discr.t option)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
        (num_elements : int)
      : (rw, int, rw NM.ListStorage.t) InnerArray.t =
      init_list ?discr ~storage_type:ListStorageType.Bytes1 ~codecs:int8_list_codecs
        struct_storage pointer_word num_elements

    let init_int16_list
        ?(discr : Discr.t option)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
        (num_elements : int)
      : (rw, int, rw NM.ListStorage.t) InnerArray.t =
      init_list ?discr ~storage_type:ListStorageType.Bytes2 ~codecs:int16_list_codecs
        struct_storage pointer_word num_elements

    let init_int32_list
        ?(discr : Discr.t option)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
        (num_elements : int)
      : (rw, int32, rw NM.ListStorage.t) InnerArray.t =
      init_list ?discr ~storage_type:ListStorageType.Bytes4 ~codecs:int32_list_codecs
        struct_storage pointer_word num_elements

    let init_int64_list
        ?(discr : Discr.t option)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
        (num_elements : int)
      : (rw, int64, rw NM.ListStorage.t) InnerArray.t =
      init_list ?discr ~storage_type:ListStorageType.Bytes8 ~codecs:int64_list_codecs
        struct_storage pointer_word num_elements

    let init_uint8_list
        ?(discr : Discr.t option)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
        (num_elements : int)
      : (rw, int, rw NM.ListStorage.t) InnerArray.t =
      init_list ?discr ~storage_type:ListStorageType.Bytes1 ~codecs:uint8_list_codecs
        struct_storage pointer_word num_elements

    let init_uint16_list
        ?(discr : Discr.t option)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
        (num_elements : int)
      : (rw, int, rw NM.ListStorage.t) InnerArray.t =
      init_list ?discr ~storage_type:ListStorageType.Bytes2 ~codecs:uint16_list_codecs
        struct_storage pointer_word num_elements

    let init_uint32_list
        ?(discr : Discr.t option)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
        (num_elements : int)
      : (rw, Uint32.t, rw NM.ListStorage.t) InnerArray.t =
      init_list ?discr ~storage_type:ListStorageType.Bytes4 ~codecs:uint32_list_codecs
        struct_storage pointer_word num_elements

    let init_uint64_list
        ?(discr : Discr.t option)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
        (num_elements : int)
      : (rw, Uint64.t, rw NM.ListStorage.t) InnerArray.t =
      init_list ?discr ~storage_type:ListStorageType.Bytes8 ~codecs:uint64_list_codecs
        struct_storage pointer_word num_elements

    let init_float32_list
        ?(discr : Discr.t option)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
        (num_elements : int)
      : (rw, float, rw NM.ListStorage.t) InnerArray.t =
      init_list ?discr ~storage_type:ListStorageType.Bytes4 ~codecs:float32_list_codecs
        struct_storage pointer_word num_elements

    let init_float64_list
        ?(discr : Discr.t option)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
        (num_elements : int)
      : (rw, float, rw NM.ListStorage.t) InnerArray.t =
      init_list ?discr ~storage_type:ListStorageType.Bytes8 ~codecs:float64_list_codecs
        struct_storage pointer_word num_elements

    let init_text_list
        ?(discr : Discr.t option)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
        (num_elements : int)
      : (rw, string, rw NM.ListStorage.t) InnerArray.t =
      init_list ?discr ~storage_type:ListStorageType.Pointer ~codecs:text_list_codecs
        struct_storage pointer_word num_elements

    let init_blob_list
        ?(discr : Discr.t option)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
        (num_elements : int)
      : (rw, string, rw NM.ListStorage.t) InnerArray.t =
      init_list ?discr ~storage_type:ListStorageType.Pointer ~codecs:blob_list_codecs
        struct_storage pointer_word num_elements

    let init_struct_list
        ?(discr : Discr.t option)
        ~(data_words : int)
        ~(pointer_words : int)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
        (num_elements : int)
      : (rw, (rw, _) NM.StructStorage.t, rw NM.ListStorage.t) InnerArray.t =
      init_list ?discr ~storage_type:(
        ListStorageType.Composite (data_words, pointer_words))
        struct_storage pointer_word ~codecs:struct_list_codecs num_elements

    let init_struct
        ?(discr : Discr.t option)
        ~(data_words : int)
        ~(pointer_words : int)
        (struct_storage : (rw, _) NM.StructStorage.t)
        (pointer_word : int)
      : (rw, _) NM.StructStorage.t =
      let pointers = struct_storage.NM.StructStorage.pointers in
      let num_pointers = pointers.NM.Slice.len / sizeof_uint64 in
      (* Struct should have already been upgraded to at least the
         expected data region and pointer region sizes *)
      assert (pointer_word < num_pointers);
      let pointer_bytes = {
        pointers with
        NM.Slice.start = pointers.NM.Slice.start + (pointer_word * sizeof_uint64);
        NM.Slice.len   = sizeof_uint64;
      } in
      let () = set_opt_discriminant struct_storage.NM.StructStorage.data discr in
      let () = BOps.deep_zero_pointer pointer_bytes in
      let storage =
        BOps.alloc_struct_storage pointer_bytes.NM.Slice.msg ~data_words ~pointer_words
      in
      let () = BOps.init_struct_pointer pointer_bytes storage in
      storage

    let init_struct_pointer
        pointer_bytes
        ~(data_words : int)
        ~(pointer_words : int)
      : (rw, _) NM.StructStorage.t =
      let () = BOps.deep_zero_pointer pointer_bytes in
      let storage =
        BOps.alloc_struct_storage pointer_bytes.NM.Slice.msg ~data_words ~pointer_words
      in
      let () = BOps.init_struct_pointer pointer_bytes storage in
      storage

    (* Locate the storage region corresponding to the root struct of a message.
       The [data_words] and [pointer_words] specify the expected struct layout. *)
    let get_root_struct
        (m : rw NM.Message.t)
        ~(data_words : int)
        ~(pointer_words : int)
      : (rw, _) NM.StructStorage.t =
      let first_segment = NM.Message.get_segment m 0 in
      if NM.Segment.length first_segment < sizeof_uint64 then
        invalid_msg "message is too small to contain root struct pointer"
      else
        let pointer_bytes = {
          NM.Slice.msg        = m;
          NM.Slice.segment    = first_segment;
          NM.Slice.segment_id = 0;
          NM.Slice.start      = 0;
          NM.Slice.len        = sizeof_uint64
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
      : (rw, _) NM.StructStorage.t =
      let act_message_size =
        let requested_size =
          match message_size with
          | Some x -> x
          | None   -> 8192
        in
        max requested_size ((data_words + pointer_words + 1) * sizeof_uint64)
      in
      let message = NM.Message.create act_message_size in
      (* Has the important side effect of reserving space in the message for
         the root struct pointer... *)
      let _ = NM.Slice.alloc message sizeof_uint64 in
      get_root_struct message ~data_words ~pointer_words

    let pointers_struct pointers =
      let data = { pointers with NM.Slice.len = 0 } in
      NM.StructStorage.v ~data ~pointers

    let cast_struct = NM.StructStorage.cast
  end
end
