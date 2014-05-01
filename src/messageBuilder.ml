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
    (* By design, this function should always be invoked after the struct
       has been upgraded to at least the expected data and pointer
       slice sizes. *)
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
    | ListStorage.Bytes1
    | ListStorage.Bytes2
    | ListStorage.Bytes4
    | ListStorage.Bytes8 ->
        (* Short data-only struct *)
        let byte_count = ListStorage.get_byte_count storage_type in
        fun i ->
          let slice = {
            storage with
            Slice.start = i * byte_count;
            Slice.len   = byte_count;
          } in
          struct_of_bytes_slice slice
    | ListStorage.Pointer ->
        (* Single-pointer struct *)
        fun i ->
          let slice = {
            storage with
            Slice.start = i * sizeof_uint64;
            Slice.len   = sizeof_uint64;
          } in
          struct_of_pointer_slice slice
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


  module StructSizes = struct
    type t = {
      data_words    : int;
      pointer_words : int;
    }
  end

  (* Given a pointer which is expected to be a list pointer, compute the
     corresponding list storage descriptor.  If the pointer is null, storage
     for a default list is immediately allocated using [alloc_default_list]. *)
  let deref_list_pointer
      ?(struct_sizes : StructSizes.t option)
      ~(create_default : rw Message.t -> rw ListStorage.t)
      (pointer_bytes : rw Slice.t)
    : rw ListStorage.t =
    match RReader.deref_list_pointer pointer_bytes with
    | None ->
        let list_storage = create_default pointer_bytes.Slice.msg in
        let () = init_list_pointer pointer_bytes list_storage in
        list_storage
    | Some list_storage ->
        begin match struct_sizes with
        | Some { StructSizes.data_words; StructSizes.pointer_words } ->
            upgrade_struct_list pointer_bytes list_storage
              ~data_words ~pointer_words
        | None ->
            list_storage
        end


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
      ~(create_default : rw Message.t -> rw StructStorage.t)
      ~(data_words : int)
      ~(pointer_words : int)
      (pointer_bytes : rw Slice.t)
    : rw StructStorage.t =
    match RReader.deref_struct_pointer pointer_bytes with
    | None ->
        let struct_storage = create_default pointer_bytes.Slice.msg in
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
          deep_copy_list ~src:src_list_storage ~dest_message:dest.Slice.msg ()
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
     minimum allocation regions for the destination struct, and may exceed the
     corresponding sizes from the [src] (for example, when fields are added
     during a schema upgrade).
  *)
  and deep_copy_struct
      ~(src : 'cap StructStorage.t)
      ~(dest_message : rw Message.t)
      ~(data_words : int)
      ~(pointer_words : int)
    : rw StructStorage.t =
    let src_data_words    = src.StructStorage.data.Slice.len / sizeof_uint64 in
    let src_pointer_words = src.StructStorage.pointers.Slice.len / sizeof_uint64 in
    let dest_data_words    = max data_words src_data_words in
    let dest_pointer_words = max pointer_words src_pointer_words in
    let dest = alloc_struct_storage dest_message
        ~data_words:dest_data_words ~pointer_words:dest_pointer_words
    in
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
     region with deep copies of the list elements.  If the [struct_sizes]
     are provided, the deep copy will create inlined structs which have
     data and pointer regions at least as large as specified. *)
  and deep_copy_list
      ?(struct_sizes : StructSizes.t option)
      ~(src : 'cap ListStorage.t)
      ~(dest_message : rw Message.t)
      ()
    : rw ListStorage.t =
    match struct_sizes with
    | Some { StructSizes.data_words; StructSizes.pointer_words } ->
        deep_copy_struct_list ~src ~dest_message
          ~data_words ~pointer_words
    | None ->
        let dest =
          alloc_list_storage dest_message src.ListStorage.storage_type
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

  (* Given a List<Struct>, allocate new (orphaned) list storage and
     deep-copy the list elements into it.  The newly-allocated list
     shall have data and pointers regions sized according to
     [data_words] and [pointer_words], to support schema upgrades;
     if the source has a larger data/pointers region, the additional
     bytes are copied as well.

     Returns: new list storage

     FIXME: are we really copying additional source bytes here?
  *)
  and deep_copy_struct_list
      ~(src : 'cap ListStorage.t)
      ~(dest_message : rw Message.t)
      ~(data_words : int)
      ~(pointer_words : int)
    : rw ListStorage.t =
    let dest_storage =
      let (dest_data_words, dest_pointer_words) =
        match src.ListStorage.storage_type with
        | ListStorage.Bytes1
        | ListStorage.Bytes2
        | ListStorage.Bytes4
        | ListStorage.Bytes8
        | ListStorage.Pointer ->
            (data_words, pointer_words)
        | ListStorage.Composite (src_data_words, src_pointer_words) ->
            (max data_words src_data_words, max pointer_words src_pointer_words)
        | ListStorage.Empty
        | ListStorage.Bit ->
          invalid_msg
            "decoded unexpected list type where List<struct> was expected"
      in
      alloc_list_storage dest_message
        (ListStorage.Composite (dest_data_words, dest_pointer_words))
        src.ListStorage.num_elements
    in
    let src_struct_of_list_index  = make_struct_of_list_index src in
    let dest_struct_of_list_index = make_struct_of_list_index dest_storage in
    for i = 0 to src.ListStorage.num_elements - 1 do
      let src_struct  = src_struct_of_list_index i in
      let dest_struct = dest_struct_of_list_index i in
      deep_copy_struct_to_dest ~src:src_struct ~dest:dest_struct
    done;
    dest_storage


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
      init_list_pointer slice new_list_storage
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
      init_list_pointer slice new_list_storage
    in
    ListCodecs.Pointer (decode, encode)

  let struct_list_codecs =
    let bytes_decoder slice =
      struct_of_bytes_slice slice
    in
    let bytes_encoder v slice =
      let dest = struct_of_bytes_slice slice in
      deep_copy_struct_to_dest ~src:v ~dest
    in
    let pointer_decoder slice =
      struct_of_pointer_slice slice
    in
    let pointer_encoder v slice =
      let dest = struct_of_pointer_slice slice in
      deep_copy_struct_to_dest ~src:v ~dest
    in
    let composite_decoder x = x in
    let composite_encoder v dest = deep_copy_struct_to_dest ~src:v ~dest in
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
      f (get_struct_pointer struct_storage pointer_word)
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
      ?(struct_sizes : StructSizes.t option)
      ?(default : ro ListStorage.t option)
      ~(storage_type : ListStorage.storage_type_t)
      ~(codecs : 'a ListCodecs.t)
      (pointer_bytes : rw Slice.t)
    : (rw, 'a, rw ListStorage.t) Runtime.Array.t =
    let create_default message =
      match default with
      | Some default_storage ->
          deep_copy_list ?struct_sizes ~src:default_storage ~dest_message:message ()
      | None ->
          alloc_list_storage message storage_type 0
    in
    let list_storage = deref_list_pointer ?struct_sizes ~create_default
        pointer_bytes
    in
    make_array_readwrite list_storage codecs

  let get_void_list
      ?(default : ro ListStorage.t option)
      (pointer_bytes : rw Slice.t)
    : (rw, unit, rw ListStorage.t) Runtime.Array.t =
    get_list ?default ~storage_type:ListStorage.Empty
      ~codecs:void_list_codecs pointer_bytes

  let get_bit_list
      ?(default : ro ListStorage.t option)
      (pointer_bytes : rw Slice.t)
    : (rw, bool, rw ListStorage.t) Runtime.Array.t =
    get_list ?default ~storage_type:ListStorage.Bit
      ~codecs:bit_list_codecs pointer_bytes

  let get_int8_list
      ?(default : ro ListStorage.t option)
      (pointer_bytes : rw Slice.t)
    : (rw, int, rw ListStorage.t) Runtime.Array.t =
    get_list ?default ~storage_type:ListStorage.Bytes1
      ~codecs:int8_list_codecs pointer_bytes

  let get_int16_list
      ?(default : ro ListStorage.t option)
      (pointer_bytes : rw Slice.t)
    : (rw, int, rw ListStorage.t) Runtime.Array.t =
    get_list ?default ~storage_type:ListStorage.Bytes2
      ~codecs:int16_list_codecs pointer_bytes

  let get_int32_list
      ?(default : ro ListStorage.t option)
      (pointer_bytes : rw Slice.t)
    : (rw, int32, rw ListStorage.t) Runtime.Array.t =
    get_list ?default ~storage_type:ListStorage.Bytes4
      ~codecs:int32_list_codecs pointer_bytes

  let get_int64_list
      ?(default : ro ListStorage.t option)
      (pointer_bytes : rw Slice.t)
    : (rw, int64, rw ListStorage.t) Runtime.Array.t =
    get_list ?default ~storage_type:ListStorage.Bytes8
      ~codecs:int64_list_codecs pointer_bytes

  let get_uint8_list
      ?(default : ro ListStorage.t option)
      (pointer_bytes : rw Slice.t)
    : (rw, int, rw ListStorage.t) Runtime.Array.t =
    get_list ?default ~storage_type:ListStorage.Bytes1
      ~codecs:uint8_list_codecs pointer_bytes

  let get_uint16_list
      ?(default : ro ListStorage.t option)
      (pointer_bytes : rw Slice.t)
    : (rw, int, rw ListStorage.t) Runtime.Array.t =
    get_list ?default ~storage_type:ListStorage.Bytes2
      ~codecs:uint16_list_codecs pointer_bytes

  let get_uint32_list
      ?(default : ro ListStorage.t option)
      (pointer_bytes : rw Slice.t)
    : (rw, Uint32.t, rw ListStorage.t) Runtime.Array.t =
    get_list ?default ~storage_type:ListStorage.Bytes4
      ~codecs:uint32_list_codecs pointer_bytes

  let get_uint64_list
      ?(default : ro ListStorage.t option)
      (pointer_bytes : rw Slice.t)
    : (rw, Uint64.t, rw ListStorage.t) Runtime.Array.t =
    get_list ?default ~storage_type:ListStorage.Bytes8
      ~codecs:uint64_list_codecs pointer_bytes

  let get_float32_list
      ?(default : ro ListStorage.t option)
      (pointer_bytes : rw Slice.t)
    : (rw, float, rw ListStorage.t) Runtime.Array.t =
    get_list ?default ~storage_type:ListStorage.Bytes4
      ~codecs:float32_list_codecs pointer_bytes

  let get_float64_list
      ?(default : ro ListStorage.t option)
      (pointer_bytes : rw Slice.t)
    : (rw, float, rw ListStorage.t) Runtime.Array.t =
    get_list ?default ~storage_type:ListStorage.Bytes8
      ~codecs:float64_list_codecs pointer_bytes

  let get_text_list
      ?(default : ro ListStorage.t option)
      (pointer_bytes : rw Slice.t)
    : (rw, string, rw ListStorage.t) Runtime.Array.t =
    get_list ?default ~storage_type:ListStorage.Pointer
      ~codecs:text_list_codecs pointer_bytes

  let get_blob_list
      ?(default : ro ListStorage.t option)
      (pointer_bytes : rw Slice.t)
    : (rw, string, rw ListStorage.t) Runtime.Array.t =
    get_list ?default ~storage_type:ListStorage.Pointer
      ~codecs:blob_list_codecs pointer_bytes

  let get_struct_list
      ?(default : ro ListStorage.t option)
      ~(data_words : int)
      ~(pointer_words : int)
      (pointer_bytes : rw Slice.t)
    : (rw, rw StructStorage.t, rw ListStorage.t) Runtime.Array.t =
    get_list ~struct_sizes:{ StructSizes.data_words; StructSizes.pointer_words }
      ?default ~storage_type:(ListStorage.Composite (data_words, pointer_words))
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
          deep_copy_struct ~src:default_storage ~dest_message:message
            ~data_words ~pointer_words
      | None ->
          alloc_struct_storage message ~data_words ~pointer_words
    in
    deref_struct_pointer ~create_default ~data_words ~pointer_words pointer_bytes


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
    let () = deep_zero_pointer pointer_bytes in
    init_list_pointer pointer_bytes new_string_storage

  let set_blob
      (value : string)
      (pointer_bytes : rw Slice.t)
    : unit =
    let new_string_storage = uint8_list_of_string
      ~null_terminated:false ~dest_message:pointer_bytes.Slice.msg
      value
    in
    let () = deep_zero_pointer pointer_bytes in
    init_list_pointer pointer_bytes new_string_storage

  let set_list_from_storage
      ?(struct_sizes : StructSizes.t option)
      ~(storage_type : ListStorage.storage_type_t)
      ~(codecs : 'a ListCodecs.t)
      (value : 'cap ListStorage.t option)
      (pointer_bytes : rw Slice.t)
    : (rw, 'a, rw ListStorage.t) Runtime.Array.t =
    let dest_storage =
      match value with
      | Some src_storage ->
          deep_copy_list ?struct_sizes
            ~src:src_storage ~dest_message:pointer_bytes.Slice.msg ()
      | None ->
          alloc_list_storage pointer_bytes.Slice.msg storage_type 0
    in
    let () = deep_zero_pointer pointer_bytes in
    let () = init_list_pointer pointer_bytes dest_storage in
    make_array_readwrite dest_storage codecs

  let set_list
      ?(struct_sizes : StructSizes.t option)
      ~(storage_type : ListStorage.storage_type_t)
      ~(codecs : 'a ListCodecs.t)
      (value : ('cap1, 'a, 'cap2 ListStorage.t) Runtime.Array.t)
      (pointer_bytes : rw Slice.t)
    : (rw, 'a, rw ListStorage.t) Runtime.Array.t =
    set_list_from_storage ?struct_sizes ~storage_type ~codecs
      (InnerArray.to_storage (InnerArray.of_outer_array value))
      pointer_bytes

  let set_void_list
      (value : ('cap1, unit, 'cap2 ListStorage.t) Runtime.Array.t)
      (pointer_bytes : rw Slice.t)
    : (rw, unit, rw ListStorage.t) Runtime.Array.t =
    set_list ~storage_type:ListStorage.Empty ~codecs:void_list_codecs
      value pointer_bytes

  let set_bit_list
      (value : ('cap1, bool, 'cap2 ListStorage.t) Runtime.Array.t)
      (pointer_bytes : rw Slice.t)
    : (rw, bool, rw ListStorage.t) Runtime.Array.t =
    set_list ~storage_type:ListStorage.Bit ~codecs:bit_list_codecs
      value pointer_bytes

  let set_int8_list
      (value : ('cap1, int, 'cap2 ListStorage.t) Runtime.Array.t)
      (pointer_bytes : rw Slice.t)
    : (rw, int, 'cap ListStorage.t) Runtime.Array.t =
    set_list ~storage_type:ListStorage.Bytes1 ~codecs:int8_list_codecs
      value pointer_bytes

  let set_int16_list
      (value : ('cap1, int, 'cap2 ListStorage.t) Runtime.Array.t)
      (pointer_bytes : rw Slice.t)
    : (rw, int, 'cap ListStorage.t) Runtime.Array.t =
    set_list ~storage_type:ListStorage.Bytes2 ~codecs:int16_list_codecs
      value pointer_bytes

  let set_int32_list
      (value : ('cap1, int32, 'cap ListStorage.t) Runtime.Array.t)
      (pointer_bytes : rw Slice.t)
    : (rw, int32, rw ListStorage.t) Runtime.Array.t =
    set_list ~storage_type:ListStorage.Bytes4 ~codecs:int32_list_codecs
      value pointer_bytes

  let set_int64_list
      (value : ('cap1, int64, 'cap2 ListStorage.t) Runtime.Array.t)
      (pointer_bytes : rw Slice.t)
    : (rw, int64, rw ListStorage.t) Runtime.Array.t =
    set_list ~storage_type:ListStorage.Bytes8 ~codecs:int64_list_codecs
      value pointer_bytes

  let set_uint8_list
      (value : ('cap1, int, 'cap2 ListStorage.t) Runtime.Array.t)
      (pointer_bytes : rw Slice.t)
    : (rw, int, rw ListStorage.t) Runtime.Array.t =
    set_list ~storage_type:ListStorage.Bytes1 ~codecs:uint8_list_codecs
      value pointer_bytes

  let set_uint16_list
      (value : ('cap1, int, 'cap2 ListStorage.t) Runtime.Array.t)
      (pointer_bytes : rw Slice.t)
    : (rw, int, rw ListStorage.t) Runtime.Array.t =
    set_list ~storage_type:ListStorage.Bytes2 ~codecs:uint16_list_codecs
      value pointer_bytes

  let set_uint32_list
      (value : ('cap1, Uint32.t, 'cap2 ListStorage.t) Runtime.Array.t)
      (pointer_bytes : rw Slice.t)
    : (rw, Uint32.t, rw ListStorage.t) Runtime.Array.t =
    set_list ~storage_type:ListStorage.Bytes4 ~codecs:uint32_list_codecs
      value pointer_bytes

  let set_uint64_list
      (value : ('cap1, Uint64.t, 'cap2 ListStorage.t) Runtime.Array.t)
      (pointer_bytes : rw Slice.t)
    : (rw, Uint64.t, rw ListStorage.t) Runtime.Array.t =
    set_list ~storage_type:ListStorage.Bytes8 ~codecs:uint64_list_codecs
      value pointer_bytes

  let set_float32_list
      (value : ('cap1, float, 'cap2 ListStorage.t) Runtime.Array.t)
      (pointer_bytes : rw Slice.t)
    : (rw, float, rw ListStorage.t) Runtime.Array.t =
    set_list ~storage_type:ListStorage.Bytes4 ~codecs:float32_list_codecs
      value pointer_bytes

  let set_float64_list
      (value : ('cap1, float, 'cap2 ListStorage.t) Runtime.Array.t)
      (pointer_bytes : rw Slice.t)
    : (rw, float, rw ListStorage.t) Runtime.Array.t =
    set_list ~storage_type:ListStorage.Bytes8 ~codecs:float64_list_codecs
      value pointer_bytes

  let set_text_list
      (value : ('cap1, string, 'cap2 ListStorage.t) Runtime.Array.t)
      (pointer_bytes : rw Slice.t)
    : (rw, string, rw ListStorage.t) Runtime.Array.t =
    set_list ~storage_type:ListStorage.Pointer ~codecs:text_list_codecs
      value pointer_bytes

  let set_blob_list
      (value : ('cap1, string, 'cap2 ListStorage.t) Runtime.Array.t)
      (pointer_bytes : rw Slice.t)
    : (rw, string, rw ListStorage.t) Runtime.Array.t =
    set_list ~storage_type:ListStorage.Pointer ~codecs:blob_list_codecs
      value pointer_bytes

  let set_struct_list
      ~(data_words : int)
      ~(pointer_words : int)
      (* FIXME: this won't allow assignment from Reader struct lists *)
      (value : ('cap1, 'cap2 StructStorage.t, 'cap2 ListStorage.t) Runtime.Array.t)
      (pointer_bytes : rw Slice.t)
    : (rw, rw StructStorage.t, rw ListStorage.t) Runtime.Array.t =
    set_list ~struct_sizes:{ StructSizes.data_words; StructSizes.pointer_words }
      ~storage_type:(ListStorage.Composite (data_words, pointer_words))
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
          deep_copy_struct ~src:src_storage
            ~dest_message:pointer_bytes.Slice.msg ~data_words ~pointer_words
      | None ->
          alloc_struct_storage pointer_bytes.Slice.msg ~data_words ~pointer_words
    in
    let () = deep_zero_pointer pointer_bytes in
    let () = init_struct_pointer pointer_bytes dest_storage in
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
      ~(storage_type : ListStorage.storage_type_t)
      ~(codecs : 'a ListCodecs.t)
      (num_elements : int)
      (pointer_bytes : rw Slice.t)
    : (rw, 'a, rw ListStorage.t) Runtime.Array.t =
    let () = deep_zero_pointer pointer_bytes in
    let message = pointer_bytes.Slice.msg in
    let list_storage = alloc_list_storage message storage_type num_elements in
    let () = init_list_pointer pointer_bytes list_storage in
    make_array_readwrite list_storage codecs

  let init_void_list
      (num_elements : int)
      (pointer_bytes : rw Slice.t)
    : (rw, unit, rw ListStorage.t) Runtime.Array.t =
    init_list ~storage_type:ListStorage.Empty ~codecs:void_list_codecs
      num_elements pointer_bytes

  let init_bit_list
      (num_elements : int)
      (pointer_bytes : rw Slice.t)
    : (rw, bool, rw ListStorage.t) Runtime.Array.t =
    init_list ~storage_type:ListStorage.Bit ~codecs:bit_list_codecs
      num_elements pointer_bytes

  let init_int8_list
      (num_elements : int)
      (pointer_bytes : rw Slice.t)
    : (rw, int, rw ListStorage.t) Runtime.Array.t =
    init_list ~storage_type:ListStorage.Bytes1 ~codecs:int8_list_codecs
      num_elements pointer_bytes

  let init_int16_list
      (num_elements : int)
      (pointer_bytes : rw Slice.t)
    : (rw, int, rw ListStorage.t) Runtime.Array.t =
    init_list ~storage_type:ListStorage.Bytes2 ~codecs:int16_list_codecs
      num_elements pointer_bytes

  let init_int32_list
      (num_elements : int)
      (pointer_bytes : rw Slice.t)
    : (rw, int32, rw ListStorage.t) Runtime.Array.t =
    init_list ~storage_type:ListStorage.Bytes4 ~codecs:int32_list_codecs
      num_elements pointer_bytes

  let init_int64_list
      (num_elements : int)
      (pointer_bytes : rw Slice.t)
    : (rw, int64, rw ListStorage.t) Runtime.Array.t =
    init_list ~storage_type:ListStorage.Bytes8 ~codecs:int64_list_codecs
      num_elements pointer_bytes

  let init_uint8_list
      (num_elements : int)
      (pointer_bytes : rw Slice.t)
    : (rw, int, rw ListStorage.t) Runtime.Array.t =
    init_list ~storage_type:ListStorage.Bytes1 ~codecs:uint8_list_codecs
      num_elements pointer_bytes

  let init_uint16_list
      (num_elements : int)
      (pointer_bytes : rw Slice.t)
    : (rw, int, rw ListStorage.t) Runtime.Array.t =
    init_list ~storage_type:ListStorage.Bytes2 ~codecs:uint16_list_codecs
      num_elements pointer_bytes

  let init_uint32_list
      (num_elements : int)
      (pointer_bytes : rw Slice.t)
    : (rw, Uint32.t, rw ListStorage.t) Runtime.Array.t =
    init_list ~storage_type:ListStorage.Bytes4 ~codecs:uint32_list_codecs
      num_elements pointer_bytes

  let init_uint64_list
      (num_elements : int)
      (pointer_bytes : rw Slice.t)
    : (rw, Uint64.t, rw ListStorage.t) Runtime.Array.t =
    init_list ~storage_type:ListStorage.Bytes8 ~codecs:uint64_list_codecs
      num_elements pointer_bytes

  let init_float32_list
      (num_elements : int)
      (pointer_bytes : rw Slice.t)
    : (rw, float, rw ListStorage.t) Runtime.Array.t =
    init_list ~storage_type:ListStorage.Bytes4 ~codecs:float32_list_codecs
      num_elements pointer_bytes

  let init_float64_list
      (num_elements : int)
      (pointer_bytes : rw Slice.t)
    : (rw, float, rw ListStorage.t) Runtime.Array.t =
    init_list ~storage_type:ListStorage.Bytes8 ~codecs:float64_list_codecs
      num_elements pointer_bytes

  let init_text_list
      (num_elements : int)
      (pointer_bytes : rw Slice.t)
    : (rw, string, rw ListStorage.t) Runtime.Array.t =
    init_list ~storage_type:ListStorage.Pointer ~codecs:text_list_codecs
      num_elements pointer_bytes

  let init_blob_list
      (num_elements : int)
      (pointer_bytes : rw Slice.t)
    : (rw, string, rw ListStorage.t) Runtime.Array.t =
    init_list ~storage_type:ListStorage.Pointer ~codecs:blob_list_codecs
      num_elements pointer_bytes

  let init_struct_list
      ~(data_words : int)
      ~(pointer_words : int)
      (num_elements : int)
      (pointer_bytes : rw Slice.t)
    : (rw, rw StructStorage.t, rw ListStorage.t) Runtime.Array.t =
    init_list ~storage_type:(ListStorage.Composite (data_words, pointer_words))
      ~codecs:struct_list_codecs num_elements pointer_bytes

  let init_struct
      ~(data_words : int)
      ~(pointer_words : int)
      (pointer_bytes : rw Slice.t)
    : rw StructStorage.t =
    let () = deep_zero_pointer pointer_bytes in
    let storage =
      alloc_struct_storage pointer_bytes.Slice.msg ~data_words ~pointer_words
    in
    let () = init_struct_pointer pointer_bytes storage in
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
        alloc_struct_storage message ~data_words ~pointer_words
      in
      deref_struct_pointer ~create_default ~data_words ~pointer_words
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


