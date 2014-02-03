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

module Make (MessageWrapper : Message.S) = struct
  module RC = RuntimeCommon.Make(MessageWrapper)
  include RC


  (* Allocate storage for a struct within the specified message. *)
  let alloc_struct_storage
      (message : rw Message.t)
      (data_words : int)
      (pointer_words : int)
    : rw StructStorage.t =
    let storage = Slice.alloc message
      ((data_words + pointer_words) * sizeof_uint64)
    in {
      StructStorage.data =
        {storage with Slice.len = data_words * sizeof_uint64};
      StructStorage.pointers = {
        storage with
        Slice.start = data.Slice.start + data.Slice.len;
        Slice.len   = pointer_words * sizeof_uint64};
    }


  (* Allocate storage for a list within the specified message. *)
  let alloc_list_storage
      (message : rw Message.t)
      (storage_type : ListStorage.storage_type_t)
      (num_elements : int)
    : rw ListStorage.t =
    let storage =
      match storage_type with
      | ListStorage.Empty ->
          Slice.alloc message 0
      | ListStorage.Bit ->
          Slice.alloc message (Util.ceil_int num_elements 8)
      | ListStorage.Bytes per_element_byte_count ->
          Slice.alloc message (num_elements * per_element_byte_count)
      | ListStorage.Pointer ->
          Slice.alloc message (num_elements * sizeof_uint64)
      | ListStorage.Composite (data_words, pointer_words) ->
          (* Composite list looks a little different from the other cases:
             content is prefixed by a tag word which describes the shape of
             the content. *)
          let word_count = 1 + (num_elements * (data_words + pointer_words)) in
          let slice = Slice.alloc message (word_count * sizeof_uint64) in
          let tag_descr = {
            StructPointer.offset = num_elements;
            StructPointer.data_size = data_words;
            StructPointer.pointers_size = pointer_words;
          } in
          let tag_val = StructPointer.encode tag_descr in
          let () = Slice.set_int64 slice 0 tag_val in
          slice
    in
    let open ListStorage in {
      storage;
      storage_type;
      num_elements;
    }


  (* Given a description of a cap'n proto far pointer, get the data associated with
     the pointer.  A far pointer can either point to a normal pointer or to a
     "landing pad" with a content pointer and a tag word; [deref_normal_pointer]
     describes the method to use for dereferencing the pointer in the former case,
     and [deref_tagged_far_pointer] describes the method to use in the latter
     case. *)
  let deref_far_pointer
      (far_pointer : FarPointer.t)
      (message : rw Message.t)
      (deref_normal_pointer     : rw Slice.t -> 'a)
      (deref_tagged_far_pointer : rw Slice.t -> 'a)
    : 'a =
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
        deref_normal_pointer next_pointer_bytes
    | TaggedFarPointer ->
        let landing_pad_bytes = {
          Slice.msg        = message;
          Slice.segment_id = far_pointer.segment_id;
          Slice.start      = far_pointer.offset * sizeof_uint64;
          Slice.len        = 2 * sizeof_uint64;
        } in
        let () = bounds_check_slice_exn
          ~err:"far pointer describes invalid tagged landing pad" landing_pad_bytes
        in
        deref_tagged_far_pointer landing_pad_bytes


  (* Given a far-pointer "landing pad" which is expected to point to list storage,
     compute the corresponding list storage descriptor. *)
  let deref_list_tagged_far_pointer
      (landing_pad_bytes : rw Slice.t)
    : rw ListStorage.t =
    let far_pointer_bytes = { landing_pad_bytes with Slice.len = sizeof_uint64 } in
    let list_tag_bytes = {
      landing_pad_bytes with
      Slice.start = Slice.get_end far_pointer_bytes;
      Slice.len   = sizeof_uint64;
    } in
    match (decode_pointer far_pointer_bytes, decode_pointer list_tag_bytes) with
    | (Pointer.Far far_pointer, Pointer.List list_pointer) ->
        make_list_storage
          ~message:landing_pad_bytes.Slice.msg
          ~segment_id:far_pointer.FarPointer.segment_id
          ~segment_offset:(far_pointer.FarPointer.offset * sizeof_uint64)
          ~list_pointer
    | _ ->
        invalid_msg "invalid type tags for list-tagged far pointer"


  (* Initialize a far pointer so that it will point to the specified [content].
     [init_normal_pointer] describes how to construct a normal intra-segment
     pointer which is appropriate for the content type; [init_far_pointer_tag]
     provides a similar method for constructing the tag word found in a
     "double far" landing pad. *)
  let init_far_pointer
      (pointer_bytes : rw Slice.t)
      (content : 'a)
      (init_normal_pointer : rw Slice.t -> 'a -> unit)
      (init_far_pointer_tag : rw Slice.t -> unit)
    : unit =
    let landing_pad_opt = Slice.alloc_in_segment
        storage_slice.Slice.msg storage_slice.Slice.segment_id sizeof_uint64
    in
    begin match landing_pad_opt with
    | Some landing_pad_bytes ->
        (* Use a "normal" far pointer. *)
        let () = init_normal_pointer landing_pad_bytes content in
        let far_pointer_desc = {
          FarPointer.landing_pad = FarPointer.NormalPointer;
          FarPointer.offset = landing_pad_bytes.Slice.start / sizeof_uint64;
          FarPointer.segment_id = landing_pad_bytes.Slice.segment_id;
        } in
        let far_pointer_val = FarPointer.encode far_pointer_desc in
        Slice.set_int64 pointer_bytes 0 far_pointer_val
    | None ->
        (* Use the "double far" convention. *)
        let landing_pad_bytes =
          let landing_pad = Slice.alloc pointer_bytes.Slice.msg (2 * sizeof_uint64) in
          let far_pointer_desc = {
            FarPointer.landing_pad = FarPointer.NormalPointer;
            FarPointer.offset = storage_slice.Slice.start / 8;
            FarPointer.segment_id = storage_slice.Slice.segment_id;
          } in
          let () = Slice.set_int64 landing_pad 0
            (FarPointer.encode far_pointer_desc)
          in
          let tag_slice = {
            landing_pad with
            Slice.start = landing_pad.Slice.start + sizeof_uint64;
            Slice.len   = sizeof_uint64;
          } in
          let () = init_far_pointer_tag tag_slice in
          landing_pad
        in
        let far_pointer_desc = {
          FarPointer.landing_pad = FarPointer.TaggedFarPointer;
          FarPointer.offset = landing_pad_bytes.Slice.start / sizeof_uint64;
          FarPointer.segment_id = landing_pad_bytes.Slice.segment_id;
        } in
        let far_pointer_val = FarPointer.encode far_pointer_desc in
        Slice.set_int64 pointer_bytes 0 far_pointer_val
    end


  let list_pointer_type_of_storage_type tp =
    match tp with
    | ListStorage.Empty       -> ListPointer.Void
    | ListStorage.Bit         -> ListPointer.OneBitValue
    | ListStorage.Bytes 1     -> ListPointer.OneByteValue
    | ListStorage.Bytes 2     -> ListPointer.TwoByteValue
    | ListStorage.Bytes 4     -> ListPointer.FourByteValue
    | ListStorage.Bytes 8     -> ListPointer.EightByteValue
    | ListStorage.Pointer     -> ListPointer.EightBytePointer
    | ListStorage.Composite _ -> ListPointer.Composite
    | ListStorage.Bytes _     -> assert false


  (* Given a pointer location and list storage located within the same
     message segment, modify the pointer so that it points to the list
     storage. *)
  let fill_normal_list_pointer
      (pointer_bytes : rw Slice.t)
      (list_storage : rw ListStorage.t)
    : unit =
    let storage_slice = list_storage.ListStorage.storage in
    let () = assert (storage_slice.Slice.segment_id = pointer_bytes.Slice.segment_id) in
    let offset_bytes = storage_slice.Slice.start - Slice.get_end pointer_bytes in
    let () = assert (offset_bytes land 7 = 0) in
    let offset_words = offset_bytes / 8 in
    let element_type =
      list_pointer_type_of_storage_type list_storage.ListStorage.storage_type
    in
    let pointer_element_count =
      match list_storage.ListStorage.storage_type with
      | Composite (data_words, pointer_words) ->
          list_storage.ListStorage.num_elements * (data_words + pointer_word)
      | _ ->
          list_storage.ListStorage.num_elements
    in
    let pointer_descr = {
      ListPointer.offset = offset_words;
      ListPointer.element_type = element_type;
      ListPointer.num_elements = pointer_element_count;
    } in
    let pointer_val = ListPointer.encode pointer_descr in
    Slice.set_int64 pointer_bytes 0 pointer_val


  (* Initialize a list pointer so that it points to the specified list storage. *)
  let init_list_pointer
      (pointer_bytes : rw Slice.t)
      (list_storage : rw ListStorage.t)
    : unit =
    let storage_slice = list_storage.ListStorage.storage in
    if storage_slice.Slice.segment_id = pointer_bytes.Slice.segment_id then
      (* Use a normal intra-segment list pointer. *)
      fill_normal_list_pointer pointer_bytes list_storage
    else
      let init_far_pointer_tag tag_slice =
        let pointer_element_count =
          match list_storage.ListStorage.storage_type with
          | Composite (data_words, pointer_words) ->
              list_storage.ListStorage.num_elements * (data_words + pointer_word)
          | _ ->
              list_storage.ListStorage.num_elements
        in
        let tag_word_desc = {
          ListPointer.offset = 0;
          ListPointer.element_type =
            list_pointer_type_of_storage_type list_storage.ListStorage.storage_type;
          ListPointer.num_elements = pointer_element_count;
        } in
        Slice.set_int64 pointer_bytes 0 (ListPointer.encode tag_word_desc)
      in
      init_far_pointer pointer_bytes list_storage fill_normal_list_pointer
        init_far_pointer_tag


  (* Given a pointer which is expected to be a list pointer, compute the corresponding
     list storage descriptor.  If the pointer is null, storage for a default list is
     immediately allocated using [alloc_default_list]. *)
  let rec deref_list_pointer
      (pointer_bytes : rw Slice.t)
      (alloc_default_list : rw Message.t -> rw ListStorage.t)
    : rw ListStorage.t =
    match decode_pointer pointer_bytes with
    | Pointer.Null ->
        let list_storage = alloc_default_list pointer_bytes.Slice.msg in
        let () = init_list_pointer pointer_bytes list_storage in
        list_storage
    | Pointer.List list_pointer ->
        Some (make_list_storage
          ~message:pointer_bytes.Slice.msg
          ~segment_id:pointer_bytes.Slice.segment_id
          ~segment_offset:((Slice.get_end pointer_bytes) +
                             (list_pointer.ListPointer.offset * sizeof_uint64))
          ~list_pointer)
    | Pointer.Far far_pointer ->
        deref_far_pointer far_pointer pointer_bytes.Slice.msg
          deref_list_pointer deref_list_tagged_far_pointer
    | Pointer.Struct _ ->
        invalid_msg "decoded struct pointer where list pointer was expected"


  (* Given a far-pointer "landing pad" which is expected to point to struct storage,
     compute the corresponding struct storage descriptor. *)
  let deref_struct_tagged_far_pointer
      (landing_pad_bytes : rw Slice.t)
    : rw StructStorage.t =
    let far_pointer_bytes = {
      landing_pad_bytes with
      Slice.len = sizeof_uint64
    } in
    let struct_tag_bytes = {
      landing_pad_bytes with
      Slice.start = Slice.get_end far_pointer_bytes;
      Slice.len   = sizeof_uint64;
    } in
    match (decode_pointer far_pointer_bytes, decode_pointer struct_tag_bytes) with
    | (Pointer.Far far_pointer, Pointer.Struct struct_pointer) ->
        let data = {
          Slice.msg        = landing_pad_bytes.Slice.msg;
          Slice.segment_id = far_pointer.FarPointer.segment_id;
          Slice.start      = far_pointer.FarPointer.offset * sizeof_uint64;
          Slice.len        = struct_pointer.StructPointer.data_size * sizeof_uint64;
        } in
        let pointers = {
          data with
          Slice.start = Slice.get_end data;
          Slice.len   = struct_pointer.StructPointer.pointers_size * sizeof_uint64;
        } in
        let () = bounds_check_slice_exn
          ~err:"struct-tagged far pointer describes invalid data region" data
        in
        let () = bounds_check_slice_exn
          ~err:"struct-tagged far pointer describes invalid pointers region" pointers
        in
        { StructStorage.data; StructStorage.pointers; }
    | _ ->
        invalid_msg "invalid type tags for struct-tagged far pointer"


  (* Given a pointer location and struct storage located within the same
     message segment, modify the pointer so that it points to the struct
     storage. *)
  let init_normal_struct_pointer
      (pointer_bytes : rw Slice.t)
      (struct_storage : rw StructStorage.t)
    : unit =
    let () = assert (struct_storage.StructStorage.data.Slice.segment_id =
      pointer_bytes.Slice.segment_id)
    in
    let pointer_descr = {
      StructPointer.offset = struct_storage.StructStorage.data.Slice.start -
          Slice.get_end pointer_bytes;
      StructPointer.data_size = struct_storage.StructStorage.data.Slice.len / 8;
      StructPointer.pointer_size = struct_storage.StructStorage.pointers.Slice.len / 8;
    } in
    let pointer_val = StructPointer.encode pointer_descr in
    Slice.set_int64 pointer_bytes 0 pointer_val


  (* Initialize a struct pointer so that it points to the specified struct storage. *)
  let init_struct_pointer
      (pointer_bytes : rw Slice.t)
      (struct_storage : rw StructStorage.t)
    : unit =
    if struct_storage.StructStorage.data.Slice.segment_id =
        pointer_bytes.Slice.segment_id then
      (* Use a normal intra-segment struct pointer. *)
      init_normal_struct_pointer pointer_bytes struct_storage
    else
      let init_far_pointer_tag tag_slice =
        let tag_word_desc = {
          StructPointer.offset = 0;
          StructPointer.data_size = struct_storage.StructStorage.data.Slice.len / 8;
          StructPointer.pointers_size = struct_storage.StructStorage.pointers.Slice.len / 8;
        } in
        Slice.set_int64 pointer_bytes 0 (StructPointer.encode tag_word_desc)
      in
      init_far_pointer pointer_bytes struct_storage init_normal_struct_pointer
        init_far_pointer_tag


  (* Given a pointer which is expected to be a struct pointer, compute the corresponding
     struct storage descriptor.  If the pointer is null, storage for a default struct
     is immediately allocated using [alloc_default_struct]. *)
  let rec deref_struct_pointer
      (pointer_bytes : rw Slice.t)
      (alloc_default_struct : rw Message.t -> rw StructStorage.t)
    : rw StructStorage.t =
    match decode_pointer pointer_bytes with
    | Pointer.Null ->
        let struct_storage = alloc_default_struct pointer_bytes.Slice.msg in
        let () = init_struct_pointer pointer_bytes struct_storage in
        struct_storage
    | Pointer.Struct struct_pointer ->
        let open StructPointer in
        let data = {
          pointer_bytes with
          Slice.start = (Slice.get_end pointer_bytes) +
                          (struct_pointer.offset * sizeof_uint64);
          Slice.len   = struct_pointer.data_size * sizeof_uint64;
        } in
        let pointers = {
          data with
          Slice.start = Slice.get_end data;
          Slice.len   = struct_pointer.pointers_size * sizeof_uint64;
        } in
        let () = bounds_check_slice_exn
          ~err:"struct pointer describes invalid data region" data
        in
        let () = bounds_check_slice_exn
          ~err:"struct pointer describes invalid pointers region" data
        in
        Some { StructStorage.data; StructStorage.pointers; }
    | Pointer.Far far_pointer ->
        deref_far_pointer far_pointer pointer_bytes.Slice.msg
          deref_struct_pointer deref_struct_tagged_far_pointer
    | Pointer.List _ ->
        invalid_msg "decoded list pointer where struct pointer was expected"

end


