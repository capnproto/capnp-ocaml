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
  module RReader = MessageReader.Make(MessageWrapper)
  module RC = RuntimeCommon.Make(MessageWrapper)
  include RC


  (* Given storage for a struct, get the pointer bytes for the given
     struct-relative pointer index. *)
  let get_struct_pointer
      (struct_storage : 'cap StructStorage.t)
      (pointer_word : int)
    : 'cap Slice.t =
    let pointers = struct_storage.StructStorage.pointers in
    let num_pointers = pointers.Slice.len / sizeof_uint64 in
    let () = assert (pointer_word < num_pointers) in {
      pointers with
      Slice.start = pointers.Slice.start + (pointer_word * sizeof_uint64);
      Slice.len   = sizeof_uint64;
    }


  (* Allocate storage for a struct within the specified message. *)
  let alloc_struct_storage
      (message : rw Message.t)
      ~(data_words : int)
      ~(pointer_words : int)
    : rw StructStorage.t =
    let storage = Slice.alloc message
      ((data_words + pointer_words) * sizeof_uint64)
    in
    let data = {storage with Slice.len = data_words * sizeof_uint64} in
    let pointers = {
      storage with
      Slice.start = data.Slice.start + data.Slice.len;
      Slice.len   = pointer_words * sizeof_uint64;
    } in {
      StructStorage.data = data;
      StructStorage.pointers = pointers;
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
      | ListStorage.Bytes1
      | ListStorage.Bytes2
      | ListStorage.Bytes4
      | ListStorage.Bytes8
      | ListStorage.Pointer ->
          Slice.alloc message
            (num_elements * (ListStorage.get_byte_count storage_type))
      | ListStorage.Composite (data_words, pointer_words) ->
          (* Composite list looks a little different from the other cases:
             content is prefixed by a tag word which describes the shape of
             the content. *)
          let word_count = 1 + (num_elements * (data_words + pointer_words)) in
          let slice = Slice.alloc message (word_count * sizeof_uint64) in
          let tag_descr = {
            StructPointer.offset = num_elements;
            StructPointer.data_words = data_words;
            StructPointer.pointer_words = pointer_words;
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


  (* Initialize a far pointer so that it will point to the specified [content],
     which is physically located in the given [content_slice].
     [init_normal_pointer] describes how to construct a normal intra-segment
     pointer which is appropriate for the content type; [init_far_pointer_tag]
     provides a similar method for constructing the tag word found in a
     "double far" landing pad. *)
  (* FIXME: need a way to get the storage slice associated with the content... *)
  let init_far_pointer
      (pointer_bytes : rw Slice.t)
      ~(content : 'a)
      ~(content_slice : rw Slice.t)
      ~(init_normal_pointer : rw Slice.t -> 'a -> unit)
      ~(init_far_pointer_tag : rw Slice.t -> unit)
    : unit =
    let landing_pad_opt = Slice.alloc_in_segment
        content_slice.Slice.msg content_slice.Slice.segment_id sizeof_uint64
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
          let landing_pad =
            Slice.alloc pointer_bytes.Slice.msg (2 * sizeof_uint64)
          in
          let far_pointer_desc = {
            FarPointer.landing_pad = FarPointer.NormalPointer;
            FarPointer.offset = content_slice.Slice.start / 8;
            FarPointer.segment_id = content_slice.Slice.segment_id;
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
    | ListStorage.Bytes1      -> ListPointer.OneByteValue
    | ListStorage.Bytes2      -> ListPointer.TwoByteValue
    | ListStorage.Bytes4      -> ListPointer.FourByteValue
    | ListStorage.Bytes8      -> ListPointer.EightByteValue
    | ListStorage.Pointer     -> ListPointer.EightBytePointer
    | ListStorage.Composite _ -> ListPointer.Composite


  (* Given a pointer location and list storage located within the same
     message segment, modify the pointer so that it points to the list
     storage. *)
  let init_normal_list_pointer
      (pointer_bytes : rw Slice.t)
      (list_storage : rw ListStorage.t)
    : unit =
    let storage_slice = list_storage.ListStorage.storage in
    let () =
      assert (storage_slice.Slice.segment_id = pointer_bytes.Slice.segment_id)
    in
    let offset_bytes = storage_slice.Slice.start - Slice.get_end pointer_bytes in
    let () = assert (offset_bytes land 7 = 0) in
    let offset_words = offset_bytes / 8 in
    let element_type =
      list_pointer_type_of_storage_type list_storage.ListStorage.storage_type
    in
    let pointer_element_count =
      match list_storage.ListStorage.storage_type with
      | ListStorage.Composite (data_words, pointer_words) ->
          list_storage.ListStorage.num_elements * (data_words + pointer_words)
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
      init_normal_list_pointer pointer_bytes list_storage
    else
      let init_far_pointer_tag tag_slice =
        let pointer_element_count =
          match list_storage.ListStorage.storage_type with
          | ListStorage.Composite (data_words, pointer_words) ->
              list_storage.ListStorage.num_elements * (data_words + pointer_words)
          | _ ->
              list_storage.ListStorage.num_elements
        in
        let tag_word_desc = {
          ListPointer.offset = 0;
          ListPointer.element_type = list_pointer_type_of_storage_type
              list_storage.ListStorage.storage_type;
          ListPointer.num_elements = pointer_element_count;
        } in
        Slice.set_int64 pointer_bytes 0 (ListPointer.encode tag_word_desc)
      in
      init_far_pointer pointer_bytes
        ~content:list_storage
        ~content_slice:list_storage.ListStorage.storage
        ~init_normal_pointer:init_normal_list_pointer
        ~init_far_pointer_tag


  (* Given a pointer which is expected to be a list pointer, compute the
     corresponding list storage descriptor.  If the pointer is null, storage
     for a default list is immediately allocated using [alloc_default_list]. *)
  let deref_list_pointer
      (pointer_bytes : rw Slice.t)
      (alloc_default_list : rw Message.t -> rw ListStorage.t)
    : rw ListStorage.t =
    match RReader.deref_list_pointer pointer_bytes with
    | None ->
        let list_storage = alloc_default_list pointer_bytes.Slice.msg in
        let () = init_list_pointer pointer_bytes list_storage in
        list_storage
    | Some list_storage ->
        list_storage


  (* Given a pointer location and struct storage located within the same
     message segment, modify the pointer so that it points to the struct
     storage. *)
  let init_normal_struct_pointer
      (pointer_bytes : rw Slice.t)
      (struct_storage : 'cap StructStorage.t)
    : unit =
    let () = assert (struct_storage.StructStorage.data.Slice.segment_id =
      pointer_bytes.Slice.segment_id)
    in
    let pointer_descr = {
      StructPointer.offset = struct_storage.StructStorage.data.Slice.start -
          Slice.get_end pointer_bytes;
      StructPointer.data_words =
        struct_storage.StructStorage.data.Slice.len / 8;
      StructPointer.pointer_words =
        struct_storage.StructStorage.pointers.Slice.len / 8;
    } in
    let pointer_val = StructPointer.encode pointer_descr in
    Slice.set_int64 pointer_bytes 0 pointer_val


  (* Initialize a struct pointer so that it points to the specified
     struct storage. *)
  let init_struct_pointer
      (pointer_bytes : rw Slice.t)
      (struct_storage : 'cap StructStorage.t)
    : unit =
    if struct_storage.StructStorage.data.Slice.segment_id =
        pointer_bytes.Slice.segment_id then
      (* Use a normal intra-segment struct pointer. *)
      init_normal_struct_pointer pointer_bytes struct_storage
    else
      let init_far_pointer_tag tag_slice =
        let tag_word_desc = {
          StructPointer.offset = 0;
          StructPointer.data_words =
            struct_storage.StructStorage.data.Slice.len / 8;
          StructPointer.pointer_words =
            struct_storage.StructStorage.pointers.Slice.len / 8;
        } in
        Slice.set_int64 pointer_bytes 0 (StructPointer.encode tag_word_desc)
      in
      let content_slice = {
        struct_storage.StructStorage.data with
        Slice.len = struct_storage.StructStorage.data.Slice.len +
            struct_storage.StructStorage.pointers.Slice.len
      } in
      init_far_pointer pointer_bytes
        ~content:struct_storage
        ~content_slice
        ~init_normal_pointer:init_normal_struct_pointer
        ~init_far_pointer_tag


  (* Copy a pointer from the source slice to the destination slice.  If the
     source and destination are in different segments, this may result in
     allocating additional message space to instantiate a far pointer. *)
  let copy_pointer
      ~(src : 'cap Slice.t)
      ~(dest : rw Slice.t)
    : unit =
    match deref_pointer src with
    | Object.None ->
        Slice.set_int64 dest 0 Int64.zero
    | Object.List list_storage ->
        init_list_pointer dest list_storage
    | Object.Struct struct_storage ->
        init_struct_pointer dest struct_storage


  let shallow_copy_struct
      ~(src : 'cap StructStorage.t)
      ~(dest : rw StructStorage.t)
    : unit =
    let open StructStorage in
    let data_copy_size =
      min src.data.Slice.len dest.data.Slice.len
    in
    let () = Slice.blit
        ~src:src.data ~src_ofs:0
        ~dest:dest.data ~dest_ofs:0
        ~len:data_copy_size
    in
    let pointer_copy_size =
      min src.pointers.Slice.len dest.pointers.Slice.len
    in
    let pointer_copy_words = pointer_copy_size / sizeof_uint64 in
    for i = 0 to pointer_copy_words - 1 do
      let src_pointer  = get_struct_pointer src i in
      let dest_pointer = get_struct_pointer dest i in
      copy_pointer ~src:src_pointer ~dest:dest_pointer
    done


  let shallow_zero_out_struct
      (struct_storage : rw StructStorage.t)
    : unit =
    let open StructStorage in
    Slice.zero_out struct_storage.data
      ~ofs:0 ~len:struct_storage.data.Slice.len;
    Slice.zero_out struct_storage.pointers
      ~ofs:0 ~len:struct_storage.pointers.Slice.len


  (* Upgrade a struct so that its data and pointer regions are at least as large
     as the protocol currently specifies.  If the [orig] struct satisfies the
     requirements of the [data_words] and [pointer_words], this is a no-op.
     Otherwise a new struct is allocated, the data is copied over, the [orig]
     is zeroed out, and the pointer to the struct is updated.

     Returns: new struct descriptor (possibly the same as the old one). *)
  let upgrade_struct
      (pointer_bytes : rw Slice.t)
      (orig : rw StructStorage.t)
      ~(data_words : int)
      ~(pointer_words : int)
    : rw StructStorage.t =
    let open StructStorage in
    if orig.data.Slice.len < data_words * sizeof_uint64 ||
       orig.pointers.Slice.len < pointer_words * sizeof_uint64 then
      let new_storage =
        alloc_struct_storage orig.data.Slice.msg ~data_words ~pointer_words
      in
      let () = shallow_copy_struct ~src:orig ~dest:new_storage in
      let () = init_struct_pointer pointer_bytes new_storage in
      let () = shallow_zero_out_struct orig in
      new_storage
    else
      orig


  (* Given a pointer which is expected to be a struct pointer, compute the
     corresponding struct storage descriptor.  If the pointer is null, storage
     for a default struct is immediately allocated using [alloc_default_struct].
     [data_words] and [pointer_words] indicate the expected structure layout;
     if the struct has a smaller layout (i.e. from an older protocol version),
     then a new struct is allocated and the data is copied over. *)
  let deref_struct_pointer
      (pointer_bytes : rw Slice.t)
      (alloc_default_struct : rw Message.t -> rw StructStorage.t)
      ~(data_words : int)
      ~(pointer_words : int)
    : rw StructStorage.t =
    match RReader.deref_struct_pointer pointer_bytes with
    | None ->
        let struct_storage = alloc_default_struct pointer_bytes.Slice.msg in
        let () = init_struct_pointer pointer_bytes struct_storage in
        struct_storage
    | Some struct_storage ->
        upgrade_struct pointer_bytes struct_storage ~data_words ~pointer_words


  (* Given a [src] pointer to an arbitrary struct or list, first create a
     deep copy of the pointed-to data then store a pointer to the data in
     [dest]. *)
  let rec deep_copy_pointer
      ~(src : 'cap Slice.t)
      ~(dest : rw Slice.t)
    : unit =
    match deref_pointer src with
    | Object.None ->
        Slice.set_int64 dest 0 Int64.zero
    | Object.List src_list_storage ->
        let dest_list_storage =
          deep_copy_list ~src:src_list_storage ~dest_message:dest.Slice.msg
        in
        init_list_pointer dest dest_list_storage
    | Object.Struct src_struct_storage ->
        let dest_struct_storage =
          let data_words =
            src_struct_storage.StructStorage.data.Slice.len / sizeof_uint64
          in
          let pointer_words =
            src_struct_storage.StructStorage.pointers.Slice.len / sizeof_uint64
          in
          deep_copy_struct ~src:src_struct_storage ~dest_message:dest.Slice.msg
            ~data_words ~pointer_words
        in
        init_struct_pointer dest dest_struct_storage

  (* Given a [src] struct storage descriptor, first allocate storage in
     [dest_message] for a copy of the struct and then fill the allocated
     region with a deep copy.  [data_words] and [pointer_words] specify the
     allocation regions for the destination struct, and may exceed the
     corresponding sizes from the [src] (for example, when fields are added
     during a schema upgrade). *)
  and deep_copy_struct
      ~(src : 'cap StructStorage.t)
      ~(dest_message : rw Message.t)
      ~(data_words : int)
      ~(pointer_words : int)
    : rw StructStorage.t =
    let dest = alloc_struct_storage dest_message ~data_words ~pointer_words in
    let () = deep_copy_struct_to_dest ~src ~dest in
    dest

  (* As [deep_copy_struct], but the destination is already allocated. *)
  and deep_copy_struct_to_dest
      ~(src : 'cap StructStorage.t)
      ~(dest : rw StructStorage.t)
    : unit =
    let () = Slice.blit
      ~src:src.StructStorage.data ~src_ofs:0
      ~dest:dest.StructStorage.data ~dest_ofs:0
      ~len:(min src.StructStorage.data.Slice.len dest.StructStorage.data.Slice.len)
    in
    let src_pointer_words =
      src.StructStorage.pointers.Slice.len / sizeof_uint64
    in
    let dest_pointer_words =
      dest.StructStorage.pointers.Slice.len / sizeof_uint64
    in
    let pointer_words = min src_pointer_words dest_pointer_words in
    for i = 0 to pointer_words - 1 do
      let open StructStorage in
      let src_pointer = {
        src.pointers with
        Slice.start = src.pointers.Slice.start + (i * sizeof_uint64);
        Slice.len   = sizeof_uint64;
      } in
      let dest_pointer = {
        dest.pointers with
        Slice.start = dest.pointers.Slice.start + (i * sizeof_uint64);
        Slice.len   = sizeof_uint64;
      } in
      deep_copy_pointer ~src:src_pointer ~dest:dest_pointer
    done

  (* Given a [src] list storage descriptor, first allocate storage in
     [dest_message] for a copy of the list and then fill the allocated
     region with deep copies of the list elements. *)
  and deep_copy_list
      ~(src : 'cap ListStorage.t)
      ~(dest_message : rw Message.t)
    : rw ListStorage.t =
    let dest = alloc_list_storage dest_message src.ListStorage.storage_type
      src.ListStorage.num_elements
    in
    let copy_by_value byte_count = Slice.blit
      ~src:src.ListStorage.storage ~src_ofs:0
      ~dest:dest.ListStorage.storage ~dest_ofs:0
      ~len:byte_count
    in
    let () =
      match src.ListStorage.storage_type with
      | ListStorage.Empty ->
          ()
      | ListStorage.Bit ->
          copy_by_value (Util.ceil_int src.ListStorage.num_elements 8)
      | ListStorage.Bytes1
      | ListStorage.Bytes2
      | ListStorage.Bytes4
      | ListStorage.Bytes8 ->
          let byte_count =
            ListStorage.get_byte_count src.ListStorage.storage_type
          in
          copy_by_value (src.ListStorage.num_elements * byte_count)
      | ListStorage.Pointer ->
          let open ListStorage in
          for i = 0 to src.num_elements - 1 do
            let src_pointer = {
              src.storage with
              Slice.start = src.storage.Slice.start + (i * sizeof_uint64);
              Slice.len   = sizeof_uint64;
            } in
            let dest_pointer = {
              dest.storage with
              Slice.start = dest.storage.Slice.start + (i * sizeof_uint64);
              Slice.len   = sizeof_uint64;
            } in
            deep_copy_pointer ~src:src_pointer ~dest:dest_pointer
          done
      | ListStorage.Composite (data_words, pointer_words) ->
          let words_per_element = data_words + pointer_words in
          let open ListStorage in
          for i = 0 to src.num_elements - 1 do
            let src_struct = {
              StructStorage.data = {
                src.storage with
                Slice.start = src.storage.Slice.start +
                    (i * words_per_element * sizeof_uint64);
                Slice.len = data_words * sizeof_uint64;};
              StructStorage.pointers = {
                src.storage with
                Slice.start = src.storage.Slice.start +
                    ((i * words_per_element) + data_words) * sizeof_uint64;
                Slice.len = pointer_words * sizeof_uint64;};
            } in
            let dest_struct = {
              StructStorage.data = {
                dest.storage with
                Slice.start = dest.storage.Slice.start +
                    (i * words_per_element * sizeof_uint64);
                Slice.len = data_words * sizeof_uint64;};
              StructStorage.pointers = {
                dest.storage with
                Slice.start = dest.storage.Slice.start +
                    ((i * words_per_element) + data_words) * sizeof_uint64;
                Slice.len = pointer_words * sizeof_uint64;};
            } in
            deep_copy_struct_to_dest ~src:src_struct ~dest:dest_struct
          done
    in
    dest


  (* Recursively zero out all data which this pointer points to.  The pointer
     value is unchanged. *)
  let rec deep_zero_pointer
      (pointer_bytes : rw Slice.t)
    : unit =
    match deref_pointer pointer_bytes with
    | Object.None ->
        ()
    | Object.List list_storage ->
        deep_zero_list list_storage
    | Object.Struct struct_storage ->
        deep_zero_struct struct_storage

  and deep_zero_list
      (list_storage : rw ListStorage.t)
    : unit =
    match list_storage.ListStorage.storage_type with
    | ListStorage.Empty
    | ListStorage.Bit
    | ListStorage.Bytes1
    | ListStorage.Bytes2
    | ListStorage.Bytes4
    | ListStorage.Bytes8 ->
        Slice.zero_out list_storage.ListStorage.storage
          ~ofs:0 ~len:list_storage.ListStorage.storage.Slice.len
    | ListStorage.Pointer ->
        let open ListStorage in
        let () =
          for i = 0 to list_storage.num_elements - 1 do
            let pointer_bytes = {
              list_storage.storage with
              Slice.start =
                list_storage.storage.Slice.start + (i * sizeof_uint64);
              Slice.len = sizeof_uint64;
            } in
            deep_zero_pointer pointer_bytes
          done
        in
        Slice.zero_out list_storage.storage
          ~ofs:0 ~len:list_storage.storage.Slice.len
    | ListStorage.Composite (data_words, pointer_words) ->
        let open ListStorage in
        let () =
          let total_words = data_words + pointer_words in
          for i = 0 to list_storage.num_elements - 1 do
            (* Note: delegating to [deep_zero_struct] is kind of inefficient
               because it means we clear most of the list twice. *)
            let data = {
              list_storage.storage with
              Slice.start = list_storage.storage.Slice.start +
                  (i * total_words * sizeof_uint64);
              Slice.len = data_words * sizeof_uint64;
            } in
            let pointers = {
              list_storage.storage with
              Slice.start = Slice.get_end data;
              Slice.len   = pointer_words * sizeof_uint64;
            } in
            deep_zero_struct { StructStorage.data; StructStorage.pointers }
          done
        in
        (* Composite lists prefix the data with a tag word, so clean up
           the tag word along with everything else *)
        let content_slice = {
          list_storage.storage with
          Slice.start = list_storage.storage.Slice.start - sizeof_uint64;
          Slice.len   = list_storage.storage.Slice.len   + sizeof_uint64;
        } in
        Slice.zero_out content_slice ~ofs:0 ~len:content_slice.Slice.len

  and deep_zero_struct
    (struct_storage : rw StructStorage.t)
    : unit =
    let open StructStorage in
    let pointer_words =
      struct_storage.pointers.Slice.len / sizeof_uint64
    in
    for i = 0 to pointer_words - 1 do
      let pointer_bytes = get_struct_pointer struct_storage i in
      deep_zero_pointer pointer_bytes
    done;
    Slice.zero_out struct_storage.data
      ~ofs:0 ~len:struct_storage.data.Slice.len;
    Slice.zero_out struct_storage.pointers
      ~ofs:0 ~len:struct_storage.pointers.Slice.len


  (* Given a string, generate an orphaned cap'n proto List<Uint8> which contains
     the string content. *)
  let uint8_list_of_string
      ~(null_terminated : bool)   (* true if the data is expected to end in 0 *)
      ~(dest_message : rw Message.t)
      (src : string)
    : rw ListStorage.t =
    let list_storage = alloc_list_storage dest_message
        ListStorage.Bytes1
        (String.length src + (if null_terminated then 1 else 0))
    in
    let (_ : int) = String.fold src ~init:0 ~f:(fun ofs c ->
        let byte = Char.to_int c in
        let () = Slice.set_uint8 list_storage.ListStorage.storage ofs byte in
        ofs + 1)
    in
    list_storage


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto Data payload. *)
  let get_struct_field_blob
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      ~(default : string)
    : string =
    let pointer_bytes = get_struct_pointer struct_storage pointer_word in
    (* Data fields are always accessed by value, not by reference, since
       we always do an immediate decode to [string].  Therefore we can
       use the Reader logic to handle this case. *)
    match RReader.deref_list_pointer pointer_bytes with
    | Some list_storage ->
        string_of_uint8_list ~null_terminated:false list_storage
    | None ->
        default


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto Text payload. *)
  let get_struct_field_text
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      ~(default : string)
    : string =
    let pointer_bytes = get_struct_pointer struct_storage pointer_word in
    (* Text fields are always accessed by value, not by reference, since
       we always do an immediate decode to [string].  Therefore we can
       use the Reader logic to handle this case. *)
    match RReader.deref_list_pointer pointer_bytes with
    | Some list_storage ->
        string_of_uint8_list ~null_terminated:true list_storage
    | None ->
        default


  let get_struct_field_list
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      ~(codecs : 'a ListCodecs.t)
      ~(alloc_default : rw Message.t -> rw ListStorage.t)
    : (rw, 'a, rw ListStorage.t) Runtime.Array.t =
    let pointer_bytes = get_struct_pointer struct_storage pointer_word in
    let list_storage = deref_list_pointer pointer_bytes alloc_default in
    make_array_readwrite list_storage codecs


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto List<Bool>. *)
  let get_struct_field_bit_list
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
    : (rw, bool, rw ListStorage.t) Runtime.Array.t =
    let codecs = ListCodecs.Bit ((fun (x : bool) -> x), (fun (x : bool) -> x)) in
    let alloc_default message = alloc_list_storage message ListStorage.Bit 0 in
    get_struct_field_list struct_storage pointer_word ~codecs ~alloc_default


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto List<Int8>. *)
  let get_struct_field_int8_list
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
    : (rw, int, rw ListStorage.t) Runtime.Array.t =
    let codecs = ListCodecs.Bytes1 (
        (fun slice -> Slice.get_int8 slice 0),
          (fun v slice -> Slice.set_int8 slice 0 v))
    in
    let alloc_default message =
      alloc_list_storage message ListStorage.Bytes1 0
    in
    get_struct_field_list struct_storage pointer_word ~codecs ~alloc_default


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto List<Int16>. *)
  let get_struct_field_int16_list
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
    : (rw, int, rw ListStorage.t) Runtime.Array.t =
    let codecs = ListCodecs.Bytes2 (
        (fun slice -> Slice.get_int16 slice 0),
          (fun v slice -> Slice.set_int16 slice 0 v))
    in
    let alloc_default message =
      alloc_list_storage message ListStorage.Bytes2 0
    in
    get_struct_field_list struct_storage pointer_word ~codecs ~alloc_default


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto List<Int32>. *)
  let get_struct_field_int32_list
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
    : (rw, int32, rw ListStorage.t) Runtime.Array.t =
    let codecs = ListCodecs.Bytes4 (
        (fun slice -> Slice.get_int32 slice 0),
          (fun v slice -> Slice.set_int32 slice 0 v))
    in
    let alloc_default message =
      alloc_list_storage message ListStorage.Bytes4 0
    in
    get_struct_field_list struct_storage pointer_word ~codecs ~alloc_default


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto List<Int64>. *)
  let get_struct_field_int64_list
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
    : (rw, int64, rw ListStorage.t) Runtime.Array.t =
    let codecs = ListCodecs.Bytes8 (
        (fun slice -> Slice.get_int64 slice 0),
          (fun v slice -> Slice.set_int64 slice 0 v))
    in
    let alloc_default message =
      alloc_list_storage message ListStorage.Bytes8 0
    in
    get_struct_field_list struct_storage pointer_word ~codecs ~alloc_default


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto List<UInt8>. *)
  let get_struct_field_uint8_list
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
    : (rw, int, rw ListStorage.t) Runtime.Array.t =
    let codecs = ListCodecs.Bytes1 (
        (fun slice -> Slice.get_uint8 slice 0),
          (fun v slice -> Slice.set_uint8 slice 0 v))
    in
    let alloc_default message =
      alloc_list_storage message ListStorage.Bytes1 0
    in
    get_struct_field_list struct_storage pointer_word ~codecs ~alloc_default


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto List<UInt16>. *)
  let get_struct_field_uint16_list
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
    : (rw, int, rw ListStorage.t) Runtime.Array.t =
    let codecs = ListCodecs.Bytes2 (
        (fun slice -> Slice.get_uint16 slice 0),
          (fun v slice -> Slice.set_uint16 slice 0 v))
    in
    let alloc_default message =
      alloc_list_storage message ListStorage.Bytes2 0
    in
    get_struct_field_list struct_storage pointer_word ~codecs ~alloc_default


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto List<UInt32>. *)
  let get_struct_field_uint32_list
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
    : (rw, Uint32.t, rw ListStorage.t) Runtime.Array.t =
    let codecs = ListCodecs.Bytes4 (
        (fun slice -> Slice.get_uint32 slice 0),
          (fun v slice -> Slice.set_uint32 slice 0 v))
    in
    let alloc_default message =
      alloc_list_storage message ListStorage.Bytes4 0
    in
    get_struct_field_list struct_storage pointer_word ~codecs ~alloc_default


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto List<UInt64>. *)
  let get_struct_field_uint64_list
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
    : (rw, Uint64.t, rw ListStorage.t) Runtime.Array.t =
    let codecs = ListCodecs.Bytes8 (
        (fun slice -> Slice.get_uint64 slice 0),
          (fun v slice -> Slice.set_uint64 slice 0 v))
    in
    let alloc_default message =
      alloc_list_storage message ListStorage.Bytes8 0
    in
    get_struct_field_list struct_storage pointer_word ~codecs ~alloc_default


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto List<Float32>. *)
  let get_struct_field_float32_list
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
    : (rw, float, rw ListStorage.t) Runtime.Array.t =
    let codecs = ListCodecs.Bytes4 (
        (fun slice -> Int32.float_of_bits (Slice.get_int32 slice 0)),
          (fun v slice -> Slice.set_int32 slice 0 (Int32.bits_of_float v)))
    in
    let alloc_default message =
      alloc_list_storage message ListStorage.Bytes4 0
    in
    get_struct_field_list struct_storage pointer_word ~codecs ~alloc_default


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto List<Float64>. *)
  let get_struct_field_float64_list
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
    : (rw, float, rw ListStorage.t) Runtime.Array.t =
    let codecs = ListCodecs.Bytes8 (
        (fun slice -> Int64.float_of_bits (Slice.get_int64 slice 0)),
          (fun v slice -> Slice.set_int64 slice 0 (Int64.bits_of_float v)))
    in
    let alloc_default message =
      alloc_list_storage message ListStorage.Bytes8 0
    in
    get_struct_field_list struct_storage pointer_word ~codecs ~alloc_default


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto List<Data>. *)
  let get_struct_field_blob_list
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
    : (rw, string, rw ListStorage.t) Runtime.Array.t =
    let codecs =
      let decode slice =
        (* In this implementation Data fields are always accessed by value,
           not by reference, since we always do an immediate decode to [string].
           Therefore we can use the Reader logic to handle this case. *)
        match RReader.deref_list_pointer slice with
        | Some list_storage ->
            string_of_uint8_list ~null_terminated:false list_storage
        | None ->
            ""
      in
      let encode v slice =
        (* Note: could avoid an allocation if the new string is not longer
           than the old one.  This deep copy strategy has the advantage of
           being correct in all cases. *)
        let new_string_storage = uint8_list_of_string
            ~null_terminated:false ~dest_message:slice.Slice.msg
            v
        in
        let () = deep_zero_pointer slice in
        init_list_pointer slice new_string_storage
      in
      ListCodecs.Pointer (decode, encode)
    in
    let alloc_default message =
      alloc_list_storage message ListStorage.Pointer 0
    in
    get_struct_field_list struct_storage pointer_word ~codecs ~alloc_default


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto List<Text>. *)
  let get_struct_field_text_list
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
    : (rw, string, rw ListStorage.t) Runtime.Array.t =
    let codecs =
      let decode slice =
        (* In this implementation Data fields are always accessed by value,
           not by reference, since we always do an immediate decode to [string].
           Therefore we can use the Reader logic to handle this case. *)
        match RReader.deref_list_pointer slice with
        | Some list_storage ->
            string_of_uint8_list ~null_terminated:true list_storage
        | None ->
            ""
      in
      let encode v slice =
        (* Note: could avoid an allocation if the new string is not longer
           than the old one.  This deep copy strategy has the advantage of
           being correct in all cases. *)
        let new_string_storage = uint8_list_of_string
            ~null_terminated:true ~dest_message:slice.Slice.msg
            v
        in
        let () = deep_zero_pointer slice in
        init_list_pointer slice new_string_storage
      in
      ListCodecs.Pointer (decode, encode)
    in
    let alloc_default message =
      alloc_list_storage message ListStorage.Pointer 0
    in
    get_struct_field_list struct_storage pointer_word ~codecs ~alloc_default


  let make_struct_of_list_index list_storage =
    let storage      = list_storage.ListStorage.storage in
    let storage_type = list_storage.ListStorage.storage_type in
    match list_storage.ListStorage.storage_type with
    | ListStorage.Bytes1
    | ListStorage.Bytes2
    | ListStorage.Bytes4
    | ListStorage.Bytes8 ->
        (* Short data-only struct *)
        let byte_count = ListStorage.get_byte_count storage_type in
        fun i ->
          let data = {
            storage with
            Slice.start = storage.Slice.start + (i * byte_count);
            Slice.len   = byte_count;
          } in
          let pointers = {
            storage with
            Slice.start = Slice.get_end data;
            Slice.len   = 0;
          } in
          { StructStorage.data; StructStorage.pointers }
    | ListStorage.Pointer ->
        (* Single-pointer struct *)
        fun i ->
          let data = {
            storage with
            Slice.start = storage.Slice.start + (i * sizeof_uint64);
            Slice.len   = 0;
          } in
          let pointers = {
            storage with
            Slice.start = Slice.get_end data;
            Slice.len   = sizeof_uint64;
          } in
          { StructStorage.data; StructStorage.pointers }
    | ListStorage.Composite (data_words, pointer_words) ->
        let data_size     = data_words * sizeof_uint64 in
        let pointers_size = pointer_words * sizeof_uint64 in
        let element_size  = data_size + pointers_size in
        fun i ->
          let data = {
            storage with
            Slice.start = storage.Slice.start + (i * element_size);
            Slice.len   = data_size;
          } in
          let pointers = {
            storage with
            Slice.start = Slice.get_end data;
            Slice.len   = pointers_size;
          } in
          { StructStorage.data; StructStorage.pointers }
    | _ ->
        invalid_msg
          "decoded unexpected list type where List<struct> was expected"


  (* Upgrade a List<Struct> so that each of the elements is at least as large
     as the requirements of the current schema version.  In general, this will
     allocate a new list, make a shallow copy of the old data into the new list,
     zero out the old data, and update the list pointer to reflect the change.
     If the schema has not changed, this is a noop.

     Returns the new list storage descriptor. *)
  let upgrade_struct_list
      (pointer_bytes : rw Slice.t)
      (list_storage : rw ListStorage.t)
      ~(data_words : int)
      ~(pointer_words : int)
    : rw ListStorage.t =
    let needs_upgrade =
      match list_storage.ListStorage.storage_type with
      | ListStorage.Bytes1
      | ListStorage.Bytes2
      | ListStorage.Bytes4
      | ListStorage.Bytes8 ->
          let orig_data_size =
            ListStorage.get_byte_count list_storage.ListStorage.storage_type
          in
          data_words * sizeof_uint64 > orig_data_size || pointer_words > 0
      | ListStorage.Pointer ->
          data_words > 0 || pointer_words > 1
      | ListStorage.Composite (orig_data_words, orig_pointer_words) ->
          data_words > orig_data_words || pointer_words > orig_pointer_words
      | ListStorage.Empty
      | ListStorage.Bit ->
          invalid_msg "decoded non-struct list where struct list was expected"
    in
    if needs_upgrade then
      let message = pointer_bytes.Slice.msg in
      let new_storage = alloc_list_storage message
          (ListStorage.Composite (data_words, pointer_words))
          list_storage.ListStorage.num_elements
      in
      let src_struct_of_index  = make_struct_of_list_index list_storage in
      let dest_struct_of_index = make_struct_of_list_index new_storage in
      for i = 0 to list_storage.ListStorage.num_elements - 1 do
        shallow_copy_struct ~src:(src_struct_of_index i)
          ~dest:(dest_struct_of_index i)
      done;
      let content_slice =
        match list_storage.ListStorage.storage_type with
        | ListStorage.Composite _ ->
            (* Composite lists prefix the storage region with a tag word,
               which we can zero out as well. *)
            { list_storage.ListStorage.storage with
              Slice.start =
                list_storage.ListStorage.storage.Slice.start - sizeof_uint64;
              Slice.len =
                list_storage.ListStorage.storage.Slice.len + sizeof_uint64; }
        | _ ->
            list_storage.ListStorage.storage
      in
      let () = init_list_pointer pointer_bytes new_storage in
      let () = Slice.zero_out content_slice
          ~ofs:0 ~len:content_slice.Slice.len
      in
      new_storage
    else
      list_storage


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto List<S> for some struct S.  The [data_words] and
     [pointer_words] describe the struct layout.  After a schema
     upgrade, it is possible that the structs stored in the list
     are smaller than expected; in this case, the data is immediately
     (shallow) copied into a new list with appropriately-sized members. *)
  let get_struct_field_struct_list
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      ~(data_words : int)
      ~(pointer_words : int)
    : (rw, rw StructStorage.t, rw ListStorage.t) Runtime.Array.t =
    let struct_codecs =
      let decode_bytes slice =
        let data = slice in
        let pointers = {
          slice with
          Slice.start = Slice.get_end slice;
          Slice.len = 0
        } in
        { StructStorage.data; StructStorage.pointers }
      in
      let encode_bytes v slice =
        let dest =
          let data = slice in
          let pointers = {
            slice with
            Slice.start = Slice.get_end slice;
            Slice.len = 0
          } in
          { StructStorage.data; StructStorage.pointers }
        in
        deep_copy_struct_to_dest ~src:v ~dest
      in
      let decode_pointer slice =
        let data = {
          slice with
          Slice.len = 0;
        } in
        let pointers = {
          slice with
          Slice.len = sizeof_uint64;
        } in
        { StructStorage.data; StructStorage.pointers }
      in
      let encode_pointer v slice =
        let dest =
          let data = {
            slice with
            Slice.len = 0;
          } in
          let pointers = {
            slice with
            Slice.len = sizeof_uint64;
          } in
          { StructStorage.data; StructStorage.pointers }
        in
        deep_copy_struct_to_dest ~src:v ~dest
      in
      let decode_composite x = x in
      let encode_composite src dest = deep_copy_struct_to_dest ~src ~dest in {
        ListCodecs.bytes = (decode_bytes, encode_bytes);
        ListCodecs.pointer = (decode_pointer, encode_pointer);
        ListCodecs.composite = (decode_composite, encode_composite);
      }
    in
    let codecs = ListCodecs.Struct struct_codecs in
    let alloc_default message = alloc_list_storage message
        (ListStorage.Composite (data_words, pointer_words)) 0
    in
    let pointer_bytes = get_struct_pointer struct_storage pointer_word in
    let orig_list_storage = deref_list_pointer pointer_bytes alloc_default in
    let list_storage = upgrade_struct_list pointer_bytes orig_list_storage
        ~data_words ~pointer_words
    in
    make_array_readwrite list_storage codecs


  (*
  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto List<L> for some list L. *)
  let get_struct_field_list_list
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      ~(decode : rw ListStorage.t ->
          (rw, 'a, rw ListStorage.t) Runtime.Array.t)
    : (rw, (rw, 'a, rw ListStorage.t) Runtime.Array.t, rw ListStorage.t) Runtime.Array.t =
    get_struct_field_bytes_list struct_storage pointer_word
      ~element_size:8
      ~decode:(fun slice ->
        let alloc_default_list message =
          alloc_list_storage message ListStorage.Pointer 0
        in
        decode (deref_list_pointer slice alloc_default_list))
      ~encode:(fun arr pointer_bytes ->
        let src_list_storage = InnerArray.to_storage arr in
        let new_list_storage = deep_copy_list ~src:src_list_storage
            ~dest_message:pointer_bytes.Slice.msg
        in
        let () = deep_zero_pointer pointer_bytes in
        init_list_pointer pointer_bytes new_list_storage)
  *)


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto List<E> for some enum E. *)
  let get_struct_field_enum_list
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      ~(decode : int -> 'a)
      ~(encode : 'a -> int)
    : (rw, 'a, rw ListStorage.t) Runtime.Array.t =
    let codecs = ListCodecs.Bytes2 (
        (fun slice -> decode (Slice.get_uint16 slice 0)),
          (fun v slice -> Slice.set_uint16 slice 0 (encode v)))
    in
    let alloc_default message =
      alloc_list_storage message ListStorage.Bytes8 0
    in
    get_struct_field_list struct_storage pointer_word ~codecs ~alloc_default


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto struct.  The [data_words] and [pointer_words] indicate
     the structure layout. *)
  let get_struct_field_struct
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      ~(data_words : int)
      ~(pointer_words : int)
    : rw StructStorage.t =
    let pointer_bytes = get_struct_pointer struct_storage pointer_word in
    let alloc_default_struct message =
      alloc_struct_storage message ~data_words ~pointer_words
    in
    deref_struct_pointer pointer_bytes alloc_default_struct
      ~data_words ~pointer_words


  (* Given storage for a struct, decode the boolean field stored
     at the given byte and bit offset within the struct's data region. *)
  let get_struct_field_bit
      (struct_storage : rw StructStorage.t)
      ~(default_bit : bool)
      ~(byte_ofs : int)
      ~(bit_ofs : int)
    : bool =
    let byte_val = Slice.get_uint8 struct_storage.StructStorage.data byte_ofs in
    let bit_val = (byte_val land (1 lsl bit_ofs)) <> 0 in
    if default_bit then not bit_val else bit_val


  (* Given storage for a struct, decode the UInt8 field stored
     at the given byte offset within the struct's data region. *)
  let get_struct_field_uint8
      (struct_storage : rw StructStorage.t)
      ~(default : int) (byte_ofs : int)
    : int =
    let numeric = Slice.get_uint8 struct_storage.StructStorage.data byte_ofs in
    numeric lxor default


  (* Given storage for a struct, decode the UInt16 field stored
     at the given byte offset within the struct's data region. *)
  let get_struct_field_uint16
      (struct_storage : rw StructStorage.t)
      ~(default : int) (byte_ofs : int)
    : int =
    let numeric = Slice.get_uint16 struct_storage.StructStorage.data byte_ofs in
    numeric lxor default


  (* Given storage for a struct, decode the UInt32 field stored
     at the given byte offset within the struct's data region. *)
  let get_struct_field_uint32
      (struct_storage : rw StructStorage.t)
      ~(default : Uint32.t) (byte_ofs : int)
    : Uint32.t =
    let numeric = Slice.get_uint32 struct_storage.StructStorage.data byte_ofs in
    Uint32.logxor numeric default


  (* Given storage for a struct, decode the UInt64 field stored
     at the given byte offset within the struct's data region. *)
  let get_struct_field_uint64
      (struct_storage : rw StructStorage.t)
      ~(default : Uint64.t) (byte_ofs : int)
    : Uint64.t =
    let numeric = Slice.get_uint64 struct_storage.StructStorage.data byte_ofs in
    Uint64.logxor numeric default


  (* Given storage for a struct, decode the Int8 field stored
     at the given byte offset within the struct's data region. *)
  let get_struct_field_int8
      (struct_storage : rw StructStorage.t)
      ~(default : int) (byte_ofs : int)
    : int =
    let numeric = Slice.get_int8 struct_storage.StructStorage.data byte_ofs in
    numeric lxor default


  (* Given storage for a struct, decode the Int16 field stored
     at the given byte offset within the struct's data region. *)
  let get_struct_field_int16
      (struct_storage : rw StructStorage.t)
      ~(default : int) (byte_ofs : int)
    : int =
    let numeric = Slice.get_int16 struct_storage.StructStorage.data byte_ofs in
    numeric lxor default


  (* Given storage for a struct, decode the Int32 field stored
     at the given byte offset within the struct's data region. *)
  let get_struct_field_int32
      (struct_storage : rw StructStorage.t)
      ~(default : int32) (byte_ofs : int)
    : Int32.t =
    let numeric = Slice.get_int32 struct_storage.StructStorage.data byte_ofs in
    Int32.bit_xor numeric default


  (* Given storage for a struct, decode the Int64 field stored
     at the given byte offset within the struct's data region. *)
  let get_struct_field_int64
      (struct_storage : rw StructStorage.t)
      ~(default : int64) (byte_ofs : int)
    : Int64.t =
    let numeric = Slice.get_int64 struct_storage.StructStorage.data byte_ofs in
    Int64.bit_xor numeric default


  module Discr = struct
    type t = {
      value    : int;
      byte_ofs : int;
    }
  end

  let rec set_discriminant
      (struct_storage : rw StructStorage.t)
      (discr : Discr.t option)
    : unit =
    match discr with
    | None ->
        ()
    | Some x ->
        set_struct_field_uint16 struct_storage ~default:0
          x.Discr.byte_ofs x.Discr.value

  (* Given storage for a struct, write a new value for the UInt16 field stored
     at the given byte offset within the struct's data region. *)
  and set_struct_field_uint16
      ?(discr : Discr.t option)
      (struct_storage : rw StructStorage.t)
      ~(default : int)
      (byte_ofs : int)
      (value : int)
    : unit =
    let () = set_discriminant struct_storage discr in
    Slice.set_uint16 struct_storage.StructStorage.data byte_ofs
      (value lxor default)



  (* Given storage for a struct, set the data for the specified struct-embedded
     pointer under the assumption that it points to a cap'n proto Data payload. *)
  let set_struct_field_blob
      ?(discr : Discr.t option)
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      (src : string)
    : unit =
    let pointer_bytes = get_struct_pointer struct_storage pointer_word in
    let new_string_storage = uint8_list_of_string
        ~null_terminated:false ~dest_message:pointer_bytes.Slice.msg
        src
    in
    let () = deep_zero_pointer pointer_bytes in
    let () = set_discriminant struct_storage discr in
    init_list_pointer pointer_bytes new_string_storage


  (* Given storage for a struct, set the data for the specified struct-embedded
     pointer under the assumption that it points to a cap'n proto Text payload. *)
  let set_struct_field_text
      ?(discr : Discr.t option)
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      (src : string)
    : unit =
    let pointer_bytes = get_struct_pointer struct_storage pointer_word in
    let new_string_storage = uint8_list_of_string
        ~null_terminated:true ~dest_message:pointer_bytes.Slice.msg
        src
    in
    let () = deep_zero_pointer pointer_bytes in
    let () = set_discriminant struct_storage discr in
    init_list_pointer pointer_bytes new_string_storage


  let set_struct_field_list
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      ~(discr : Discr.t option)
      ~(src : ('cap, 'a, 'cap ListStorage.t) Runtime.Array.t)
      ~(codecs : 'a ListCodecs.t)
      ~(alloc_default : rw Message.t -> rw ListStorage.t)
    : (rw, 'a, rw ListStorage.t) Runtime.Array.t =
    let list_storage =
      let message = struct_storage.StructStorage.pointers.Slice.msg in
      let src = InnerArray.of_outer_array src in
      let src_storage_opt = InnerArray.to_storage src in
      match src_storage_opt with
      | Some src_storage ->
          deep_copy_list ~src:src_storage ~dest_message:message
      | None ->
          alloc_default message
    in
    let pointer_bytes = get_struct_pointer struct_storage pointer_word in
    let () = deep_zero_pointer pointer_bytes in
    let () = set_discriminant struct_storage discr in
    let () = init_list_pointer pointer_bytes list_storage in
    get_struct_field_list struct_storage pointer_word ~codecs ~alloc_default


  let set_struct_field_bit_list
      ?(discr : Discr.t option)
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      (src : ('cap, bool, 'cap ListStorage.t) Runtime.Array.t)
    : (rw, bool, rw ListStorage.t) Runtime.Array.t =
    let codecs = ListCodecs.Bit ((fun (x : bool) -> x), (fun (x : bool) -> x)) in
    let alloc_default message = alloc_list_storage message ListStorage.Bit 0 in
    set_struct_field_list struct_storage pointer_word ~discr ~src
      ~codecs ~alloc_default

  let set_struct_field_int8_list
      ?(discr : Discr.t option)
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      (src : ('cap, int, 'cap ListStorage.t) Runtime.Array.t)
    : (rw, int, rw ListStorage.t) Runtime.Array.t =
    let codecs = ListCodecs.Bytes1 (
        (fun slice -> Slice.get_int8 slice 0),
          (fun v slice -> Slice.set_int8 slice 0 v))
    in
    let alloc_default message =
      alloc_list_storage message ListStorage.Bytes1 0
    in
    set_struct_field_list struct_storage pointer_word ~discr ~src
      ~codecs ~alloc_default

  let set_struct_field_int16_list
      ?(discr : Discr.t option)
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      (src : ('cap, int, 'cap ListStorage.t) Runtime.Array.t)
    : (rw, int, rw ListStorage.t) Runtime.Array.t =
    let codecs = ListCodecs.Bytes2 (
        (fun slice -> Slice.get_int16 slice 0),
          (fun v slice -> Slice.set_int16 slice 0 v))
    in
    let alloc_default message =
      alloc_list_storage message ListStorage.Bytes2 0
    in
    set_struct_field_list struct_storage pointer_word ~discr ~src
      ~codecs ~alloc_default

  let set_struct_field_int32_list
      ?(discr : Discr.t option)
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      (src : ('cap, int32, 'cap ListStorage.t) Runtime.Array.t)
    : (rw, int32, rw ListStorage.t) Runtime.Array.t =
    let codecs = ListCodecs.Bytes4 (
        (fun slice -> Slice.get_int32 slice 0),
          (fun v slice -> Slice.set_int32 slice 0 v))
    in
    let alloc_default message =
      alloc_list_storage message ListStorage.Bytes4 0
    in
    set_struct_field_list struct_storage pointer_word ~discr ~src
      ~codecs ~alloc_default

  let set_struct_field_int64_list
      ?(discr : Discr.t option)
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      (src : ('cap, int64, 'cap ListStorage.t) Runtime.Array.t)
    : (rw, int64, rw ListStorage.t) Runtime.Array.t =
    let codecs = ListCodecs.Bytes8 (
        (fun slice -> Slice.get_int64 slice 0),
          (fun v slice -> Slice.set_int64 slice 0 v))
    in
    let alloc_default message =
      alloc_list_storage message ListStorage.Bytes8 0
    in
    set_struct_field_list struct_storage pointer_word ~discr ~src
      ~codecs ~alloc_default

  let set_struct_field_uint8_list
      ?(discr : Discr.t option)
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      (src : ('cap, int, 'cap ListStorage.t) Runtime.Array.t)
    : (rw, int, rw ListStorage.t) Runtime.Array.t =
    let codecs = ListCodecs.Bytes1 (
        (fun slice -> Slice.get_uint8 slice 0),
          (fun v slice -> Slice.set_uint8 slice 0 v))
    in
    let alloc_default message =
      alloc_list_storage message ListStorage.Bytes1 0
    in
    set_struct_field_list struct_storage pointer_word ~discr ~src
      ~codecs ~alloc_default

  let set_struct_field_uint16_list
      ?(discr : Discr.t option)
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      (src : ('cap, int, 'cap ListStorage.t) Runtime.Array.t)
    : (rw, int, rw ListStorage.t) Runtime.Array.t =
    let codecs = ListCodecs.Bytes2 (
        (fun slice -> Slice.get_uint16 slice 0),
          (fun v slice -> Slice.set_uint16 slice 0 v))
    in
    let alloc_default message =
      alloc_list_storage message ListStorage.Bytes2 0
    in
    set_struct_field_list struct_storage pointer_word ~discr ~src
      ~codecs ~alloc_default

  let set_struct_field_uint32_list
      ?(discr : Discr.t option)
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      (src : ('cap, Uint32.t, 'cap ListStorage.t) Runtime.Array.t)
    : (rw, Uint32.t, rw ListStorage.t) Runtime.Array.t =
    let codecs = ListCodecs.Bytes4 (
        (fun slice -> Slice.get_uint32 slice 0),
          (fun v slice -> Slice.set_uint32 slice 0 v))
    in
    let alloc_default message =
      alloc_list_storage message ListStorage.Bytes4 0
    in
    set_struct_field_list struct_storage pointer_word ~discr ~src
      ~codecs ~alloc_default

  let set_struct_field_uint64_list
      ?(discr : Discr.t option)
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      (src : ('cap, Uint64.t, 'cap ListStorage.t) Runtime.Array.t)
    : (rw, Uint64.t, rw ListStorage.t) Runtime.Array.t =
    let codecs = ListCodecs.Bytes8 (
        (fun slice -> Slice.get_uint64 slice 0),
          (fun v slice -> Slice.set_uint64 slice 0 v))
    in
    let alloc_default message =
      alloc_list_storage message ListStorage.Bytes8 0
    in
    set_struct_field_list struct_storage pointer_word ~discr ~src
      ~codecs ~alloc_default

  let set_struct_field_float32_list
      ?(discr : Discr.t option)
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      (src : ('cap, float, 'cap ListStorage.t) Runtime.Array.t)
    : (rw, float, rw ListStorage.t) Runtime.Array.t =
    let codecs = ListCodecs.Bytes4 (
        (fun slice -> Int32.float_of_bits (Slice.get_int32 slice 0)),
          (fun v slice -> Slice.set_int32 slice 0 (Int32.bits_of_float v)))
    in
    let alloc_default message =
      alloc_list_storage message ListStorage.Bytes4 0
    in
    set_struct_field_list struct_storage pointer_word ~discr ~src
      ~codecs ~alloc_default

  let set_struct_field_float64_list
      ?(discr : Discr.t option)
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      (src : ('cap, float, 'cap ListStorage.t) Runtime.Array.t)
    : (rw, float, rw ListStorage.t) Runtime.Array.t =
    let codecs = ListCodecs.Bytes8 (
        (fun slice -> Int64.float_of_bits (Slice.get_int64 slice 0)),
          (fun v slice -> Slice.set_int64 slice 0 (Int64.bits_of_float v)))
    in
    let alloc_default message =
      alloc_list_storage message ListStorage.Bytes8 0
    in
    set_struct_field_list struct_storage pointer_word ~discr ~src
      ~codecs ~alloc_default

  let set_struct_field_blob_list
      ?(discr : Discr.t option)
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      (src : ('cap, string, 'cap ListStorage.t) Runtime.Array.t)
    : (rw, string, rw ListStorage.t) Runtime.Array.t =
    let codecs =
      let decode slice =
        (* In this implementation Data fields are always accessed by value,
           not by reference, since we always do an immediate decode to [string].
           Therefore we can use the Reader logic to handle this case. *)
        match RReader.deref_list_pointer slice with
        | Some list_storage ->
            string_of_uint8_list ~null_terminated:false list_storage
        | None ->
            ""
      in
      let encode v slice =
        (* Note: could avoid an allocation if the new string is not longer
           than the old one.  This deep copy strategy has the advantage of
           being correct in all cases. *)
        let new_string_storage = uint8_list_of_string
            ~null_terminated:false ~dest_message:slice.Slice.msg
            v
        in
        let () = deep_zero_pointer slice in
        init_list_pointer slice new_string_storage
      in
      ListCodecs.Pointer (decode, encode)
    in
    let alloc_default message =
      alloc_list_storage message ListStorage.Pointer 0
    in
    set_struct_field_list struct_storage pointer_word ~discr ~src
      ~codecs ~alloc_default

  let set_struct_field_text_list
      ?(discr : Discr.t option)
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      (src : ('cap, string, 'cap ListStorage.t) Runtime.Array.t)
    : (rw, string, rw ListStorage.t) Runtime.Array.t =
    let codecs =
      let decode slice =
        (* In this implementation Data fields are always accessed by value,
           not by reference, since we always do an immediate decode to [string].
           Therefore we can use the Reader logic to handle this case. *)
        match RReader.deref_list_pointer slice with
        | Some list_storage ->
            string_of_uint8_list ~null_terminated:true list_storage
        | None ->
            ""
      in
      let encode v slice =
        (* Note: could avoid an allocation if the new string is not longer
           than the old one.  This deep copy strategy has the advantage of
           being correct in all cases. *)
        let new_string_storage = uint8_list_of_string
            ~null_terminated:true ~dest_message:slice.Slice.msg
            v
        in
        let () = deep_zero_pointer slice in
        init_list_pointer slice new_string_storage
      in
      ListCodecs.Pointer (decode, encode)
    in
    let alloc_default message =
      alloc_list_storage message ListStorage.Pointer 0
    in
    set_struct_field_list struct_storage pointer_word ~discr ~src
      ~codecs ~alloc_default

  let set_struct_field_struct_list
      ?(discr : Discr.t option)
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      (src : ('cap, 'cap StructStorage.t option, 'cap ListStorage.t) Runtime.Array.t)
      ~(data_words : int)
      ~(pointer_words : int)
    : (rw, rw StructStorage.t, rw ListStorage.t) Runtime.Array.t =
    let struct_codecs =
      let decode_bytes slice =
        let data = slice in
        let pointers = {
          slice with
          Slice.start = Slice.get_end slice;
          Slice.len = 0
        } in
        { StructStorage.data; StructStorage.pointers }
      in
      let encode_bytes v slice =
        let dest =
          let data = slice in
          let pointers = {
            slice with
            Slice.start = Slice.get_end slice;
            Slice.len = 0
          } in
          { StructStorage.data; StructStorage.pointers }
        in
        deep_copy_struct_to_dest ~src:v ~dest
      in
      let decode_pointer slice =
        let data = {
          slice with
          Slice.len = 0;
        } in
        let pointers = {
          slice with
          Slice.len = sizeof_uint64;
        } in
        { StructStorage.data; StructStorage.pointers }
      in
      let encode_pointer v slice =
        let dest =
          let data = {
            slice with
            Slice.len = 0;
          } in
          let pointers = {
            slice with
            Slice.len = sizeof_uint64;
          } in
          { StructStorage.data; StructStorage.pointers }
        in
        deep_copy_struct_to_dest ~src:v ~dest
      in
      let decode_composite x = x in
      let encode_composite src dest = deep_copy_struct_to_dest ~src ~dest in {
        ListCodecs.bytes = (decode_bytes, encode_bytes);
        ListCodecs.pointer = (decode_pointer, encode_pointer);
        ListCodecs.composite = (decode_composite, encode_composite);
      }
    in
    let codecs = ListCodecs.Struct struct_codecs in
    let alloc_default message = alloc_list_storage message
        (ListStorage.Composite (data_words, pointer_words)) 0
    in
    let list_storage =
      let message = struct_storage.StructStorage.pointers.Slice.msg in
      let src = InnerArray.of_outer_array src in
      let src_storage_opt = InnerArray.to_storage src in
      match src_storage_opt with
      | Some src_storage ->
          deep_copy_list ~src:src_storage ~dest_message:message
      | None ->
          alloc_default message
    in
    let pointer_bytes = get_struct_pointer struct_storage pointer_word in
    let () = deep_zero_pointer pointer_bytes in
    let () = set_discriminant struct_storage discr in
    let () = init_list_pointer pointer_bytes list_storage in
    (* Note: this is pretty inefficient... first making a copy of the original,
       then upgrading the copy. *)
    let _ = upgrade_struct_list pointer_bytes list_storage
        ~data_words ~pointer_words
    in
    get_struct_field_list struct_storage pointer_word ~codecs ~alloc_default

  (*
  let set_struct_field_list_list
      ?(discr : Discr.t option)
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      (src : 'cap ListStorage.t option)
    : (rw, rw ListStorage.t, rw ListStorage.t) Runtime.Array.t =
    let () = set_struct_field_list ~discr ~storage_type:ListStorage.Pointer
        struct_storage pointer_word src
    in
    get_struct_field_list_list struct_storage pointer_word
  *)

  let set_struct_field_enum_list
      ?(discr : Discr.t option)
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      (src : ('cap, 'a, 'cap ListStorage.t) Runtime.Array.t)
      ~(decode : int -> 'a)
      ~(encode : 'a -> int)
    : (rw, 'a, rw ListStorage.t) Runtime.Array.t =
    let codecs = ListCodecs.Bytes2 (
        (fun slice -> decode (Slice.get_uint16 slice 0)),
          (fun v slice -> Slice.set_uint16 slice 0 (encode v)))
    in
    let alloc_default message =
      alloc_list_storage message ListStorage.Bytes8 0
    in
    set_struct_field_list struct_storage pointer_word ~discr ~src
      ~codecs ~alloc_default

  (* Given storage for a struct, set the data for the specified struct-embedded
     pointer equal to a deep copy of the specified [src] struct. *)
  let set_struct_field_struct
      ?(discr : Discr.t option)
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      (src : 'cap StructStorage.t option)
      ~(data_words : int)
      ~(pointer_words : int)
    : rw StructStorage.t =
    let pointer_bytes = get_struct_pointer struct_storage pointer_word in
    let new_storage =
      let message = struct_storage.StructStorage.pointers.Slice.msg in
      match src with
      | Some src' ->
          deep_copy_struct ~src:src'
            ~dest_message:message ~data_words ~pointer_words
      | None ->
          alloc_struct_storage message ~data_words ~pointer_words
    in
    let () = deep_zero_pointer pointer_bytes in
    let () = set_discriminant struct_storage discr in
    let () = init_struct_pointer pointer_bytes new_storage in
    get_struct_field_struct struct_storage pointer_word
      ~data_words ~pointer_words


  (* Given storage for a struct, write a new value for the boolean field stored
     at the given byte and bit offset within the struct's data region. *)
  let set_struct_field_bit
      ?(discr : Discr.t option)
      (struct_storage : rw StructStorage.t)
      ~(default_bit : bool)
      ~(byte_ofs : int)
      ~(bit_ofs : int)
      (value : bool)
    : unit =
    let default = if default_bit then 1 else 0 in
    let bit_val = if value then 1 else 0 in
    let bit = default lxor bit_val in
    let byte_val = Slice.get_uint8 struct_storage.StructStorage.data byte_ofs in
    let byte_val = byte_val land (lnot (1 lsl bit_ofs)) in
    let byte_val = byte_val lor (bit lsl bit_ofs) in
    let () = set_discriminant struct_storage discr in
    Slice.set_uint8 struct_storage.StructStorage.data byte_ofs byte_val


  (* Given storage for a struct, write a new value for the UInt8 field stored
     at the given byte offset within the struct's data region. *)
  let set_struct_field_uint8
      ?(discr : Discr.t option)
      (struct_storage : rw StructStorage.t)
      ~(default : int)
      (byte_ofs : int)
      (value : int)
    : unit =
    let () = set_discriminant struct_storage discr in
    Slice.set_uint8 struct_storage.StructStorage.data byte_ofs
      (value lxor default)


  (* Given storage for a struct, write a new value for the UInt32 field stored
     at the given byte offset within the struct's data region. *)
  let set_struct_field_uint32
      ?(discr : Discr.t option)
      (struct_storage : rw StructStorage.t)
      ~(default : Uint32.t)
      (byte_ofs : int)
      (value : Uint32.t)
    : unit =
    let () = set_discriminant struct_storage discr in
    Slice.set_uint32 struct_storage.StructStorage.data byte_ofs
      (Uint32.logxor value default)


  (* Given storage for a struct, write a new value for the UInt64 field stored
     at the given byte offset within the struct's data region. *)
  let set_struct_field_uint64
      ?(discr : Discr.t option)
      (struct_storage : rw StructStorage.t)
      ~(default : Uint64.t)
      (byte_ofs : int)
      (value : Uint64.t)
    : unit =
    let () = set_discriminant struct_storage discr in
    Slice.set_uint64 struct_storage.StructStorage.data byte_ofs
      (Uint64.logxor value default)


  (* Given storage for a struct, write a new value for the Int8 field stored
     at the given byte offset within the struct's data region. *)
  let set_struct_field_int8
      ?(discr : Discr.t option)
      (struct_storage : rw StructStorage.t)
      ~(default : int)
      (byte_ofs : int)
      (value : int)
    : unit =
    let () = set_discriminant struct_storage discr in
    Slice.set_int8 struct_storage.StructStorage.data byte_ofs
      (value lxor default)


  (* Given storage for a struct, write a new value for the Int16 field stored
     at the given byte offset within the struct's data region. *)
  let set_struct_field_int16
      ?(discr : Discr.t option)
      (struct_storage : rw StructStorage.t)
      ~(default : int)
      (byte_ofs : int)
      (value : int)
    : unit =
    let () = set_discriminant struct_storage discr in
    Slice.set_int16 struct_storage.StructStorage.data byte_ofs
      (value lxor default)


  (* Given storage for a struct, write a new value for the Int32 field stored
     at the given byte offset within the struct's data region. *)
  let set_struct_field_int32
      ?(discr : Discr.t option)
      (struct_storage : rw StructStorage.t)
      ~(default : int32)
      (byte_ofs : int)
      (value : int32)
    : unit =
    let () = set_discriminant struct_storage discr in
    Slice.set_int32 struct_storage.StructStorage.data byte_ofs
      (Int32.bit_xor value default)


  (* Given storage for a struct, write a new value for the Int64 field stored
     at the given byte offset within the struct's data region. *)
  let set_struct_field_int64
      ?(discr : Discr.t option)
      (struct_storage : rw StructStorage.t)
      ~(default : int64)
      (byte_ofs : int)
      (value : int64)
    : unit =
    let () = set_discriminant struct_storage discr in
    Slice.set_int64 struct_storage.StructStorage.data byte_ofs
      (Int64.bit_xor value default)


  (* Given storage for a struct, allocate a new List<T> member for one of the
     pointers stored within the struct. *)
  let init_struct_field_list
      ?(discr : Discr.t option)
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      ~(storage_type : ListStorage.storage_type_t)
      ~(num_elements : int)
    : unit =
    if num_elements < 0 then
      invalid_arg "index out of bounds"
    else
      let pointer_bytes = get_struct_pointer struct_storage pointer_word in
      let dest_message  = struct_storage.StructStorage.pointers.Slice.msg in
      let list_storage  = alloc_list_storage dest_message
          storage_type num_elements
      in
      let () = deep_zero_pointer pointer_bytes in
      let () = set_discriminant struct_storage discr in
      init_list_pointer pointer_bytes list_storage


  (* Given storage for a struct, allocate a new List<Bool> member for one
     of the pointers stored within the struct.  Returns an array builder
     for the resulting list. *)
  let init_struct_field_bit_list
      ?(discr : Discr.t option)
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      ~(num_elements : int)
    : (rw, bool, rw ListStorage.t) Runtime.Array.t =
    let () = set_discriminant struct_storage discr in
    let () = init_struct_field_list struct_storage pointer_word
        ~storage_type:ListStorage.Bit ~num_elements
    in
    get_struct_field_bit_list struct_storage pointer_word


  (* Given storage for a struct, allocate a new List<Int8> member for
     one of the pointers stored within the struct.  Returns an array
     builder for the resulting list. *)
  let init_struct_field_int8_list
      ?(discr : Discr.t option)
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      ~(num_elements : int)
    : (rw, int, rw ListStorage.t) Runtime.Array.t =
    let () = set_discriminant struct_storage discr in
    let () = init_struct_field_list struct_storage pointer_word
        ~storage_type:ListStorage.Bytes1 ~num_elements
    in
    get_struct_field_int8_list struct_storage pointer_word


  (* Given storage for a struct, allocate a new List<Int16> member for
     one of the pointers stored within the struct.  Returns an array
     builder for the resulting list. *)
  let init_struct_field_int16_list
      ?(discr : Discr.t option)
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      ~(num_elements : int)
    : (rw, int, rw ListStorage.t) Runtime.Array.t =
    let () = set_discriminant struct_storage discr in
    let () = init_struct_field_list struct_storage pointer_word
        ~storage_type:ListStorage.Bytes2 ~num_elements
    in
    get_struct_field_int16_list struct_storage pointer_word


  (* Given storage for a struct, allocate a new List<Int32> member for
     one of the pointers stored within the struct.  Returns an array
     builder for the resulting list. *)
  let init_struct_field_int32_list
      ?(discr : Discr.t option)
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      ~(num_elements : int)
    : (rw, int32, rw ListStorage.t) Runtime.Array.t =
    let () = set_discriminant struct_storage discr in
    let () = init_struct_field_list struct_storage pointer_word
        ~storage_type:ListStorage.Bytes4 ~num_elements
    in
    get_struct_field_int32_list struct_storage pointer_word


  (* Given storage for a struct, allocate a new List<Int64> member for
     one of the pointers stored within the struct.  Returns an array
     builder for the resulting list. *)
  let init_struct_field_int64_list
      ?(discr : Discr.t option)
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      ~(num_elements : int)
    : (rw, int64, rw ListStorage.t) Runtime.Array.t =
    let () = set_discriminant struct_storage discr in
    let () = init_struct_field_list struct_storage pointer_word
        ~storage_type:ListStorage.Bytes8 ~num_elements
    in
    get_struct_field_int64_list struct_storage pointer_word


  (* Given storage for a struct, allocate a new List<UInt8> member for
     one of the pointers stored within the struct.  Returns an array
     builder for the resulting list. *)
  let init_struct_field_uint8_list
      ?(discr : Discr.t option)
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      ~(num_elements : int)
    : (rw, int, rw ListStorage.t) Runtime.Array.t =
    let () = set_discriminant struct_storage discr in
    let () = init_struct_field_list struct_storage pointer_word
        ~storage_type:ListStorage.Bytes1 ~num_elements
    in
    get_struct_field_uint8_list struct_storage pointer_word


  (* Given storage for a struct, allocate a new List<UInt16> member for
     one of the pointers stored within the struct.  Returns an array
     builder for the resulting list. *)
  let init_struct_field_uint16_list
      ?(discr : Discr.t option)
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      ~(num_elements : int)
    : (rw, int, rw ListStorage.t) Runtime.Array.t =
    let () = set_discriminant struct_storage discr in
    let () = init_struct_field_list struct_storage pointer_word
        ~storage_type:ListStorage.Bytes2 ~num_elements
    in
    get_struct_field_uint16_list struct_storage pointer_word


  (* Given storage for a struct, allocate a new List<UInt32> member for
     one of the pointers stored within the struct.  Returns an array
     builder for the resulting list. *)
  let init_struct_field_uint32_list
      ?(discr : Discr.t option)
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      ~(num_elements : int)
    : (rw, Uint32.t, rw ListStorage.t) Runtime.Array.t =
    let () = set_discriminant struct_storage discr in
    let () = init_struct_field_list struct_storage pointer_word
        ~storage_type:ListStorage.Bytes4 ~num_elements
    in
    get_struct_field_uint32_list struct_storage pointer_word


  (* Given storage for a struct, allocate a new List<UInt64> member for
     one of the pointers stored within the struct.  Returns an array
     builder for the resulting list. *)
  let init_struct_field_uint64_list
      ?(discr : Discr.t option)
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      ~(num_elements : int)
    : (rw, Uint64.t, rw ListStorage.t) Runtime.Array.t =
    let () = set_discriminant struct_storage discr in
    let () = init_struct_field_list struct_storage pointer_word
        ~storage_type:ListStorage.Bytes8 ~num_elements
    in
    get_struct_field_uint64_list struct_storage pointer_word


  (* Given storage for a struct, allocate a new List<Float32> member for
     one of the pointers stored within the struct.  Returns an array
     builder for the resulting list. *)
  let init_struct_field_float32_list
      ?(discr : Discr.t option)
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      ~(num_elements : int)
    : (rw, float, rw ListStorage.t) Runtime.Array.t =
    let () = set_discriminant struct_storage discr in
    let () = init_struct_field_list struct_storage pointer_word
        ~storage_type:ListStorage.Bytes4 ~num_elements
    in
    get_struct_field_float32_list struct_storage pointer_word


  (* Given storage for a struct, allocate a new List<Float64> member for
     one of the pointers stored within the struct.  Returns an array
     builder for the resulting list. *)
  let init_struct_field_float64_list
      ?(discr : Discr.t option)
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      ~(num_elements : int)
    : (rw, float, rw ListStorage.t) Runtime.Array.t =
    let () = set_discriminant struct_storage discr in
    let () = init_struct_field_list struct_storage pointer_word
        ~storage_type:ListStorage.Bytes8 ~num_elements
    in
    get_struct_field_float64_list struct_storage pointer_word


  (* Given storage for a struct, allocate a new List<List<Data>> member for
     one of the pointers stored within the struct.  Returns an array
     builder for the resulting list. *)
  let init_struct_field_blob_list
      ?(discr : Discr.t option)
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      ~(num_elements : int)
    : (rw, string, rw ListStorage.t) Runtime.Array.t =
    let () = set_discriminant struct_storage discr in
    let () = init_struct_field_list struct_storage pointer_word
        ~storage_type:ListStorage.Pointer ~num_elements
    in
    get_struct_field_blob_list struct_storage pointer_word


  (* Given storage for a struct, allocate a new List<List<Text>> member for
     one of the pointers stored within the struct.  Returns an array
     builder for the resulting list. *)
  let init_struct_field_text_list
      ?(discr : Discr.t option)
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      ~(num_elements : int)
    : (rw, string, rw ListStorage.t) Runtime.Array.t =
    let () = set_discriminant struct_storage discr in
    let () = init_struct_field_list struct_storage pointer_word
        ~storage_type:ListStorage.Pointer ~num_elements
    in
    get_struct_field_text_list struct_storage pointer_word


  (* Given storage for a struct, allocate a new List<T> member for one of the
     pointers stored within the struct, where T is a struct type.  Returns
     an array builder for the resulting list. *)
  let init_struct_field_struct_list
      ?(discr : Discr.t option)
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      ~(num_elements : int)
      ~(data_words : int)
      ~(pointer_words : int)
    : (rw, rw StructStorage.t, rw ListStorage.t) Runtime.Array.t =
    let () = set_discriminant struct_storage discr in
    let () = init_struct_field_list struct_storage pointer_word
        ~storage_type:(ListStorage.Composite (data_words, pointer_words))
        ~num_elements
    in
    get_struct_field_struct_list struct_storage pointer_word
      ~data_words ~pointer_words


  (*
  (* Given storage for a struct, allocate a new List<List<T>> member for
     one of the pointers stored within the struct.  Returns an array builder
     for the resulting list. *)
  let init_struct_field_list_list
      ?(discr : Discr.t option)
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      ~(num_elements : int)
    : (rw ListStorage.t, 'b) Runtime.Array.t =
    let () = set_discriminant struct_storage discr in
    let () = init_struct_field_list struct_storage pointer_word
        ~storage_type:ListStorage.Pointer ~num_elements
    in
    get_struct_field_list_list struct_storage pointer_word
  *)


  (* Given storage for a struct, allocate a new List<Enum> member for
     one of the pointers stored within the struct.  Returns an array builder
     for the resulting list. *)
  let init_struct_field_enum_list
      ?(discr : Discr.t option)
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      ~(num_elements : int)
      ~(decode : int -> 'a)
      ~(encode : 'a -> int)
    : (rw, 'a, rw ListStorage.t) Runtime.Array.t =
    let () = set_discriminant struct_storage discr in
    let () = init_struct_field_list struct_storage pointer_word
        ~storage_type:ListStorage.Bytes2 ~num_elements
    in
    get_struct_field_enum_list struct_storage pointer_word
      ~decode ~encode


  (* Given storage for a parent struct, allocate a new child struct and link it
     to one of the pointers stored within the parent.  Returns a reference to
     the newly-created struct. *)
  let init_struct_field_struct
      ?(discr : Discr.t option)
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      ~(data_words : int)
      ~(pointer_words : int)
    : rw StructStorage.t =
    let pointer_bytes = get_struct_pointer struct_storage pointer_word in
    let () = deep_zero_pointer pointer_bytes in
    let alloc_default_struct message =
      alloc_struct_storage message ~data_words ~pointer_words
    in
    let () = set_discriminant struct_storage discr in
    deref_struct_pointer pointer_bytes alloc_default_struct
      ~data_words ~pointer_words


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
      let alloc_default_struct message =
        alloc_struct_storage message ~data_words ~pointer_words
      in
      deref_struct_pointer pointer_bytes alloc_default_struct
        ~data_words ~pointer_words


  (* Allocate a new message of at least the given [message_size], creating a
     root struct with the specified struct layout.
     Returns: newly-allocated root struct storage *)
  let alloc_root_struct
      ~(data_words : int)
      ~(pointer_words : int)
      ~(message_size : int)
    : rw StructStorage.t =
    let message_size =
      max message_size ((data_words + pointer_words + 1) * sizeof_uint64)
    in
    let message = Message.create message_size in
    (* Has the important side effect of reserving space in the message for
       the root struct pointer... *)
    let _ = Slice.alloc message sizeof_uint64 in
    get_root_struct message ~data_words ~pointer_words

end


