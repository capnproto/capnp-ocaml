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

(* Runtime support for Reader interfaces.  None of the functions provided
   here will modify the underlying message; derefencing null pointers and
   reading from truncated structs both lead to default data being returned. *)

open Core.Std

module Make (MessageWrapper : Message.S) = struct
  module RC = RuntimeCommon.Make(MessageWrapper)
  include RC


  (* Given a pointer which is expected to be a list pointer, compute the corresponding
     list storage descriptor.  Returns None if the pointer is null. *)
  let deref_list_pointer (pointer_bytes : 'cap Slice.t) : 'cap ListStorage.t option =
    match deref_pointer pointer_bytes with
    | Object.None ->
        None
    | Object.List list_descr ->
        Some list_descr
    | Object.Struct _ ->
        invalid_msg "decoded struct pointer where list pointer was expected"


  (* Given a pointer which is expected to be a struct pointer, compute the corresponding
     struct storage descriptor.  Returns None if the pointer is null. *)
  let deref_struct_pointer (pointer_bytes : 'cap Slice.t) : 'cap StructStorage.t option =
    match deref_pointer pointer_bytes with
    | Object.None ->
        None
    | Object.Struct struct_descr ->
        Some struct_descr
    | Object.List _ ->
        invalid_msg "decoded list pointer where struct pointer was expected"


  (* Given storage for a struct, get the pointer bytes for the given
     struct-relative pointer index.  Returns None if the requested
     pointer bytes are not backed by physical storage (i.e. the struct
     was specified by a null pointer, or the specified pointer word
     lies at an offset which is not present in the struct). *)
  let get_struct_pointer
      (struct_storage : 'cap StructStorage.t option)
      (pointer_word : int)
    : 'cap Slice.t option =
    match struct_storage with
    | Some storage ->
        StructStorage.get_pointer storage pointer_word
    | None ->
        None


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto Data payload. *)
  let get_struct_field_blob
      (struct_storage : 'cap StructStorage.t option)
      (pointer_word : int)
      ~(default : string)
    : string =
    match get_struct_pointer struct_storage pointer_word with
    | Some pointer_bytes ->
        begin match deref_list_pointer pointer_bytes with
        | Some list_storage ->
            string_of_uint8_list ~null_terminated:false list_storage
        | None ->
            default
        end
    | None ->
        default


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto Text payload. *)
  let get_struct_field_text
      (struct_storage : 'cap StructStorage.t option)
      (pointer_word : int)
      ~(default : string)
    : string =
    match get_struct_pointer struct_storage pointer_word with
    | Some pointer_bytes ->
        begin match deref_list_pointer pointer_bytes with
        | Some list_storage ->
            string_of_uint8_list ~null_terminated:true list_storage
        | None ->
            default
        end
    | None ->
        default


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto List<Bool>. *)
  let get_struct_field_bit_list
      (struct_storage : 'cap StructStorage.t option)
      (pointer_word : int)
    : ('a, 'b) Runtime.Array.t =
    match get_struct_pointer struct_storage pointer_word with
    | Some slice ->
        begin match deref_list_pointer slice with
        | Some list_storage ->
            Runtime.Array.make
              ~length:(fun x -> x.ListStorage.num_elements)
              ~get:BitList.get
              list_storage
        | None ->
            Runtime.Array.make_default ()
        end
    | None ->
        Runtime.Array.make_default ()


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto list encoded as packed bytes (e.g. List<UInt32>).  The
     provided [convert] is used to decode packed bytes as list elements. *)
  let get_struct_field_bytes_list
      (struct_storage : 'cap StructStorage.t option)
      (pointer_word : int)
      (convert : 'cap Slice.t -> 'a)
    : ('a, 'b) Runtime.Array.t =
    match get_struct_pointer struct_storage pointer_word with
    | Some slice ->
        begin match deref_list_pointer slice with
        | Some list_storage ->
            Runtime.Array.make
              ~length:(fun x -> x.ListStorage.num_elements)
              ~get:(fun x i -> convert (BytesList.get x i))
              list_storage
        | None ->
            Runtime.Array.make_default ()
        end
    | None ->
        Runtime.Array.make_default ()


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto List<Int8>. *)
  let get_struct_field_int8_list
      (struct_storage : 'cap StructStorage.t option)
      (pointer_word : int)
    : (int, 'b) Runtime.Array.t =
    get_struct_field_bytes_list struct_storage pointer_word
      (fun slice -> Slice.get_int8 slice 0)


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto List<Int16>. *)
  let get_struct_field_int16_list
      (struct_storage : 'cap StructStorage.t option)
      (pointer_word : int)
    : (int, 'b) Runtime.Array.t =
    get_struct_field_bytes_list struct_storage pointer_word
      (fun slice -> Slice.get_int16 slice 0)


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto List<Int32>. *)
  let get_struct_field_int32_list
      (struct_storage : 'cap StructStorage.t option)
      (pointer_word : int)
    : (int32, 'b) Runtime.Array.t =
    get_struct_field_bytes_list struct_storage pointer_word
      (fun slice -> Slice.get_int32 slice 0)


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto List<Int64>. *)
  let get_struct_field_int64_list
      (struct_storage : 'cap StructStorage.t option)
      (pointer_word : int)
    : (int64, 'b) Runtime.Array.t =
    get_struct_field_bytes_list struct_storage pointer_word
      (fun slice -> Slice.get_int64 slice 0)


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto List<UInt8>. *)
  let get_struct_field_uint8_list
      (struct_storage : 'cap StructStorage.t option)
      (pointer_word : int)
    : (int, 'b) Runtime.Array.t =
    get_struct_field_bytes_list struct_storage pointer_word
      (fun slice -> Slice.get_uint8 slice 0)


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto List<UInt16>. *)
  let get_struct_field_uint16_list
      (struct_storage : 'cap StructStorage.t option)
      (pointer_word : int)
    : (int, 'b) Runtime.Array.t =
    get_struct_field_bytes_list struct_storage pointer_word
      (fun slice -> Slice.get_uint16 slice 0)


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto List<UInt32>. *)
  let get_struct_field_uint32_list
      (struct_storage : 'cap StructStorage.t option)
      (pointer_word : int)
    : (Uint32.t, 'b) Runtime.Array.t =
    get_struct_field_bytes_list struct_storage pointer_word
      (fun slice -> Slice.get_uint32 slice 0)


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto List<UInt64>. *)
  let get_struct_field_uint64_list
      (struct_storage : 'cap StructStorage.t option)
      (pointer_word : int)
    : (Uint64.t, 'b) Runtime.Array.t =
    get_struct_field_bytes_list struct_storage pointer_word
      (fun slice -> Slice.get_uint64 slice 0)


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto List<Float32>. *)
  let get_struct_field_float32_list
      (struct_storage : 'cap StructStorage.t option)
      (pointer_word : int)
    : (float, 'b) Runtime.Array.t =
    get_struct_field_bytes_list struct_storage pointer_word
      (fun slice -> Int32.float_of_bits (Slice.get_int32 slice 0))


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto List<Float64>. *)
  let get_struct_field_float64_list
      (struct_storage : 'cap StructStorage.t option)
      (pointer_word : int)
    : (float, 'b) Runtime.Array.t =
    get_struct_field_bytes_list struct_storage pointer_word
      (fun slice -> Int64.float_of_bits (Slice.get_int64 slice 0))


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto List<Text>. *)
  let get_struct_field_text_list
      (struct_storage : 'cap StructStorage.t option)
      (pointer_word : int)
    : (string, 'b) Runtime.Array.t =
    get_struct_field_bytes_list struct_storage pointer_word (fun slice ->
      match deref_list_pointer slice with
      | Some list_storage ->
          string_of_uint8_list ~null_terminated:true list_storage
      | None ->
          "")


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto List<Data>. *)
  let get_struct_field_blob_list
      (struct_storage : 'cap StructStorage.t option)
      (pointer_word : int)
    : (string, 'b) Runtime.Array.t =
    get_struct_field_bytes_list struct_storage pointer_word (fun slice ->
      match deref_list_pointer slice with
      | Some list_storage ->
          string_of_uint8_list ~null_terminated:false list_storage
      | None ->
          "")


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto List<S> for some struct S. *)
  let get_struct_field_struct_list
      (struct_storage : 'cap StructStorage.t option)
      (pointer_word : int)
    : ('a, 'b) Runtime.Array.t =
    match get_struct_pointer struct_storage pointer_word with
    | Some slice ->
        begin match deref_list_pointer slice with
        | Some list_storage ->
            Runtime.Array.make
              ~length:(fun x -> x.ListStorage.num_elements)
              ~get:(fun x i -> Some (StructList.get x i))
              list_storage
        | None ->
            Runtime.Array.make_default ()
        end
    | None ->
        Runtime.Array.make_default ()


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto List<L> for some list L. *)
  let get_struct_field_list_list
      (struct_storage : 'cap StructStorage.t option)
      (pointer_word : int)
    : ('cap ListStorage.t option, 'b) Runtime.Array.t =
    get_struct_field_bytes_list struct_storage pointer_word
      (fun slice -> deref_list_pointer slice)


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto List<E> for some enum E. *)
  let get_struct_field_enum_list
      (struct_storage : 'cap StructStorage.t option)
      (pointer_word : int)
      (convert : int -> 'a)
    : ('a, 'b) Runtime.Array.t =
    get_struct_field_bytes_list struct_storage pointer_word
      (fun slice -> convert (Slice.get_uint16 slice 0))


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto struct. *)
  let get_struct_field_struct
      (struct_storage : 'cap StructStorage.t option)
      (pointer_word : int)
    : 'cap StructStorage.t option =
    match get_struct_pointer struct_storage pointer_word with
    | Some pointer_bytes -> deref_struct_pointer pointer_bytes
    | None -> None


  (* Given storage for a struct, decode the boolean field stored
     at the given byte and bit offset within the struct's data region. *)
  let get_struct_field_bit
      (struct_storage : 'cap StructStorage.t option)
      ~(default_bit : bool)
      (byte_ofs : int) (bit_ofs : int)
    : bool =
    let byte_val =
      match struct_storage with
      | Some { StructStorage.data = data; _ } when byte_ofs < data.Slice.len ->
          Slice.get_uint8 data byte_ofs
      | _ ->
          0
    in
    let bit_val = (byte_val land (1 lsl bit_ofs)) <> 0 in
    if default_bit then not bit_val else bit_val


  (* Given storage for a struct, decode the UInt8 field stored
     at the given byte offset within the struct's data region. *)
  let get_struct_field_uint8
      (struct_storage : 'cap StructStorage.t option)
      ~(default : int) (byte_ofs : int)
    : int =
    let numeric =
      match struct_storage with
      | Some { StructStorage.data = data; _ } when byte_ofs < data.Slice.len ->
          Slice.get_uint8 data byte_ofs
      | _ ->
          0
    in
    numeric lxor default


  (* Given storage for a struct, decode the UInt16 field stored
     at the given byte offset within the struct's data region. *)
  let get_struct_field_uint16
      (struct_storage : 'cap StructStorage.t option)
      ~(default : int) (byte_ofs : int)
    : int =
    let numeric =
      match struct_storage with
      | Some { StructStorage.data = data; _ } when byte_ofs < data.Slice.len - 1 ->
          Slice.get_uint16 data byte_ofs
      | _ ->
          0
    in
    numeric lxor default


  (* Given storage for a struct, decode the UInt32 field stored
     at the given byte offset within the struct's data region. *)
  let get_struct_field_uint32
      (struct_storage : 'cap StructStorage.t option)
      ~(default : Uint32.t) (byte_ofs : int)
    : Uint32.t =
    let numeric =
      match struct_storage with
      | Some { StructStorage.data = data; _ } when byte_ofs < data.Slice.len - 3 ->
          Slice.get_uint32 data byte_ofs
      | _ ->
          Uint32.zero
    in
    Uint32.logxor numeric default


  (* Given storage for a struct, decode the UInt64 field stored
     at the given byte offset within the struct's data region. *)
  let get_struct_field_uint64
      (struct_storage : 'cap StructStorage.t option)
      ~(default : Uint64.t) (byte_ofs : int)
    : Uint64.t =
    let numeric =
      match struct_storage with
      | Some { StructStorage.data = data; _ } when byte_ofs < data.Slice.len - 7 ->
          Slice.get_uint64 data byte_ofs
      | _ ->
          Uint64.zero
    in
    Uint64.logxor numeric default


  (* Given storage for a struct, decode the Int8 field stored
     at the given byte offset within the struct's data region. *)
  let get_struct_field_int8
      (struct_storage : 'cap StructStorage.t option)
      ~(default : int) (byte_ofs : int)
    : int =
    let numeric =
      match struct_storage with
      | Some { StructStorage.data = data; _ } when byte_ofs < data.Slice.len ->
          Slice.get_int8 data byte_ofs
      | _ ->
          0
    in
    numeric lxor default


  (* Given storage for a struct, decode the Int16 field stored
     at the given byte offset within the struct's data region. *)
  let get_struct_field_int16
      (struct_storage : 'cap StructStorage.t option)
      ~(default : int) (byte_ofs : int)
    : int =
    let numeric =
      match struct_storage with
      | Some { StructStorage.data = data; _ } when byte_ofs < data.Slice.len - 1 ->
          Slice.get_int16 data byte_ofs
      | _ ->
          0
    in
    numeric lxor default


  (* Given storage for a struct, decode the Int32 field stored
     at the given byte offset within the struct's data region. *)
  let get_struct_field_int32
      (struct_storage : 'cap StructStorage.t option)
      ~(default : int32) (byte_ofs : int)
    : Int32.t =
    let numeric =
      match struct_storage with
      | Some { StructStorage.data = data; _ } when byte_ofs < data.Slice.len - 3 ->
          Slice.get_int32 data byte_ofs
      | _ ->
          Int32.zero
    in
    Int32.bit_xor numeric default


  (* Given storage for a struct, decode the Int64 field stored
     at the given byte offset within the struct's data region. *)
  let get_struct_field_int64
      (struct_storage : 'cap StructStorage.t option)
      ~(default : int64) (byte_ofs : int)
    : Int64.t =
    let numeric =
      match struct_storage with
      | Some { StructStorage.data = data; _ } when byte_ofs < data.Slice.len - 7 ->
          Slice.get_int64 data byte_ofs
      | _ ->
          Int64.zero
    in
    Int64.bit_xor numeric default


  (* Locate the storage region corresponding to the root struct of a message. *)
  let get_root_struct (m : 'cap Message.t) : 'cap StructStorage.t option =
    let first_segment = Message.get_segment m 0 in
    if Segment.length first_segment < sizeof_uint64 then
      None
    else
      let pointer_bytes = {
        Slice.msg        = m;
        Slice.segment_id = 0;
        Slice.start      = 0;
        Slice.len        = sizeof_uint64
      } in
      deref_struct_pointer pointer_bytes

end

