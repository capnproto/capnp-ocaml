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
  module Reader = MessageReader.Make(MessageWrapper)
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
          let landing_pad = Slice.alloc pointer_bytes.Slice.msg (2 * sizeof_uint64) in
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
  let init_normal_list_pointer
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
          ListPointer.element_type =
            list_pointer_type_of_storage_type list_storage.ListStorage.storage_type;
          ListPointer.num_elements = pointer_element_count;
        } in
        Slice.set_int64 pointer_bytes 0 (ListPointer.encode tag_word_desc)
      in
      init_far_pointer pointer_bytes
        ~content:list_storage
        ~content_slice:list_storage.ListStorage.storage
        ~init_normal_pointer:init_normal_list_pointer
        ~init_far_pointer_tag


  (* Given a pointer which is expected to be a list pointer, compute the corresponding
     list storage descriptor.  If the pointer is null, storage for a default list is
     immediately allocated using [alloc_default_list]. *)
  let deref_list_pointer
      (pointer_bytes : rw Slice.t)
      (alloc_default_list : rw Message.t -> rw ListStorage.t)
    : rw ListStorage.t =
    match Reader.deref_list_pointer pointer_bytes with
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
      StructPointer.data_words = struct_storage.StructStorage.data.Slice.len / 8;
      StructPointer.pointer_words = struct_storage.StructStorage.pointers.Slice.len / 8;
    } in
    let pointer_val = StructPointer.encode pointer_descr in
    Slice.set_int64 pointer_bytes 0 pointer_val


  (* Initialize a struct pointer so that it points to the specified struct storage. *)
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
          StructPointer.data_words = struct_storage.StructStorage.data.Slice.len / 8;
          StructPointer.pointer_words = struct_storage.StructStorage.pointers.Slice.len / 8;
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


  (* Given a pointer which is expected to be a struct pointer, compute the corresponding
     struct storage descriptor.  If the pointer is null, storage for a default struct
     is immediately allocated using [alloc_default_struct].  [data_words] and
     [pointer_words] indicate the expected structure layout; if the struct has a
     smaller layout (i.e. from an older protocol version), then a new struct is
     allocated and the data is copied over. *)
  let deref_struct_pointer
      (pointer_bytes : rw Slice.t)
      (alloc_default_struct : rw Message.t -> rw StructStorage.t)
      ~(data_words : int)
      ~(pointer_words : int)
    : rw StructStorage.t =
    match Reader.deref_struct_pointer pointer_bytes with
    | None ->
        let struct_storage = alloc_default_struct pointer_bytes.Slice.msg in
        let () = init_struct_pointer pointer_bytes struct_storage in
        struct_storage
    | Some struct_storage ->
        struct_storage


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
          deep_copy_struct ~src:src_struct_storage ~dest_message:dest.Slice.msg
        in
        init_struct_pointer dest dest_struct_storage

  (* Given a [src] struct storage descriptor, first allocate storage in
     [dest_message] for a copy of the struct and then fill the allocated
     region with a deep copy. *)
  and deep_copy_struct
      ~(src : 'cap StructStorage.t)
      ~(dest_message : rw Message.t)
    : rw StructStorage.t =
    let data_words    = src.StructStorage.data.Slice.len / sizeof_uint64 in
    let pointer_words = src.StructStorage.pointers.Slice.len / sizeof_uint64 in
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
      ~len:dest.StructStorage.data.Slice.len
    in
    let pointer_words = dest.StructStorage.pointers.Slice.len / sizeof_uint64 in
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
      | ListStorage.Bytes byte_count ->
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
    | ListStorage.Bytes _ ->
        Slice.zero_out list_storage.ListStorage.storage
          ~ofs:0 ~len:list_storage.ListStorage.storage.Slice.len
    | ListStorage.Pointer ->
        let open ListStorage in
        let () =
          for i = 0 to list_storage.num_elements - 1 do
            let pointer_bytes = {
              list_storage.storage with
              Slice.start = list_storage.storage.Slice.start + (i * sizeof_uint64);
              Slice.len   = sizeof_uint64;
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


  (* Upgrade a struct so that its data and pointer regions are at least as large
     as the protocol currently specifies.  If the [orig] struct satisfies the
     requirements of the [data_words] and [pointer_words], this is a no-op.
     Otherwise a new struct is allocated, the data is copied over, and the [orig]
     is zeroed out.

     Returns: new struct descriptor (possibly the same as the old one). *)
  let upgrade_struct
      ~(data_words : int)
      ~(pointer_words : int)
      (orig : rw StructStorage.t)
    : rw StructStorage.t =
    let open StructStorage in
    if orig.data.Slice.len < data_words * sizeof_uint64 ||
       orig.pointers.Slice.len < pointer_words * sizeof_uint64 then
      let new_storage =
        alloc_struct_storage orig.data.Slice.msg ~data_words ~pointer_words
      in
      let () =
        let data_copy_size = min (data_words * sizeof_uint64) orig.data.Slice.len in
        let () = Slice.blit
            ~src:orig.data ~src_ofs:0
            ~dest:new_storage.data ~dest_ofs:0
            ~len:data_copy_size
        in
        let pointer_copy_words =
          min pointer_words (orig.pointers.Slice.len / sizeof_uint64)
        in
        for i = 0 to pointer_copy_words - 1 do
          let src  = get_struct_pointer orig i in
          let dest = get_struct_pointer new_storage i in
          copy_pointer ~src ~dest
        done
      in
      let () = deep_zero_struct orig in
      new_storage
    else
      orig

  (* Given a string, generate an orphaned cap'n proto List<Uint8> which contains
     the string content. *)
  let uint8_list_of_string
      ~(null_terminated : bool)   (* true if the data is expected to end in 0 *)
      ~(dest_message : rw Message.t)
      (src : string)
    : rw ListStorage.t =
    let list_storage = alloc_list_storage dest_message
        (ListStorage.Bytes 1)
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
    match Reader.deref_list_pointer pointer_bytes with
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
    match Reader.deref_list_pointer pointer_bytes with
    | Some list_storage ->
        string_of_uint8_list ~null_terminated:true list_storage
    | None ->
        default


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto List<Bool>. *)
  let get_struct_field_bit_list
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
    : ('a, 'b) Runtime.BArray.t =
    let pointer_bytes = get_struct_pointer struct_storage pointer_word in
    let alloc_default_list message =
      alloc_list_storage message ListStorage.Bit 0
    in
    let list_storage = deref_list_pointer pointer_bytes alloc_default_list in
    Runtime.BArray.make
      ~length:(fun x -> x.ListStorage.num_elements)
      ~get:BitList.get
      ~set:BitList.set
      list_storage


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto list encoded as packed bytes (e.g. List<UInt32>).
     The size of each list element in bytes is provided as [element_size].
     The [decode] function is used to decode packed bytes as list elements,
     and the [encode] function performs the opposite operation. *)
  let get_struct_field_bytes_list
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      ~(element_size : int)
      ~(decode : rw Slice.t -> 'a)
      ~(encode : 'a -> rw Slice.t -> unit)
    : ('a, 'b) Runtime.BArray.t =
    let pointer_bytes = get_struct_pointer struct_storage pointer_word in
    let alloc_default_list message =
      alloc_list_storage message (ListStorage.Bytes element_size) 0
    in
    let list_storage = deref_list_pointer pointer_bytes alloc_default_list in
    Runtime.BArray.make
      ~length:(fun x -> x.ListStorage.num_elements)
      ~get:(fun x i -> decode (BytesList.get x i))
      ~set:(fun x i v -> encode v (BytesList.get x i))
      list_storage


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto List<Int8>. *)
  let get_struct_field_int8_list
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
    : (int, 'b) Runtime.BArray.t =
    get_struct_field_bytes_list struct_storage pointer_word
      ~element_size:1
      ~decode:(fun slice -> Slice.get_int8 slice 0)
      ~encode:(fun v slice -> Slice.set_int8 slice 0 v)


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto List<Int16>. *)
  let get_struct_field_int16_list
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
    : (int, 'b) Runtime.BArray.t =
    get_struct_field_bytes_list struct_storage pointer_word
      ~element_size:2
      ~decode:(fun slice -> Slice.get_int16 slice 0)
      ~encode:(fun v slice -> Slice.set_int16 slice 0 v)


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto List<Int32>. *)
  let get_struct_field_int32_list
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
    : (int32, 'b) Runtime.BArray.t =
    get_struct_field_bytes_list struct_storage pointer_word
      ~element_size:4
      ~decode:(fun slice -> Slice.get_int32 slice 0)
      ~encode:(fun v slice -> Slice.set_int32 slice 0 v)


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto List<Int64>. *)
  let get_struct_field_int64_list
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
    : (int64, 'b) Runtime.BArray.t =
    get_struct_field_bytes_list struct_storage pointer_word
      ~element_size:8
      ~decode:(fun slice -> Slice.get_int64 slice 0)
      ~encode:(fun v slice -> Slice.set_int64 slice 0 v)


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto List<UInt8>. *)
  let get_struct_field_uint8_list
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
    : (int, 'b) Runtime.BArray.t =
    get_struct_field_bytes_list struct_storage pointer_word
      ~element_size:1
      ~decode:(fun slice -> Slice.get_uint8 slice 0)
      ~encode:(fun v slice -> Slice.set_uint8 slice 0 v)


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto List<UInt16>. *)
  let get_struct_field_uint16_list
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
    : (int, 'b) Runtime.BArray.t =
    get_struct_field_bytes_list struct_storage pointer_word
      ~element_size:2
      ~decode:(fun slice -> Slice.get_uint16 slice 0)
      ~encode:(fun v slice -> Slice.set_uint16 slice 0 v)


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto List<UInt32>. *)
  let get_struct_field_uint32_list
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
    : (Uint32.t, 'b) Runtime.BArray.t =
    get_struct_field_bytes_list struct_storage pointer_word
      ~element_size:4
      ~decode:(fun slice -> Slice.get_uint32 slice 0)
      ~encode:(fun v slice -> Slice.set_uint32 slice 0 v)


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto List<UInt64>. *)
  let get_struct_field_uint64_list
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
    : (Uint64.t, 'b) Runtime.BArray.t =
    get_struct_field_bytes_list struct_storage pointer_word
      ~element_size:8
      ~decode:(fun slice -> Slice.get_uint64 slice 0)
      ~encode:(fun v slice -> Slice.set_uint64 slice 0 v)


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto List<Float32>. *)
  let get_struct_field_float32_list
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
    : (float, 'b) Runtime.BArray.t =
    get_struct_field_bytes_list struct_storage pointer_word
      ~element_size:4
      ~decode:(fun slice -> Int32.float_of_bits (Slice.get_int32 slice 0))
      ~encode:(fun v slice -> Slice.set_int32 slice 0 (Int32.bits_of_float v))


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto List<Float64>. *)
  let get_struct_field_float64_list
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
    : (float, 'b) Runtime.BArray.t =
    get_struct_field_bytes_list struct_storage pointer_word
      ~element_size:8
      ~decode:(fun slice -> Int64.float_of_bits (Slice.get_int64 slice 0))
      ~encode:(fun v slice -> Slice.set_int64 slice 0 (Int64.bits_of_float v))


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto List<Data>. *)
  let get_struct_field_blob_list
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
    : (string, 'b) Runtime.BArray.t =
    get_struct_field_bytes_list struct_storage pointer_word
      ~element_size:8
      ~decode:(fun slice ->
        let pointer_bytes = get_struct_pointer struct_storage pointer_word in
        (* Data fields are always accessed by value, not by reference, since
           we always do an immediate decode to [string].  Therefore we can
           use the Reader logic to handle this case. *)
        match Reader.deref_list_pointer pointer_bytes with
        | Some list_storage ->
            string_of_uint8_list ~null_terminated:false list_storage
        | None ->
            "")
      ~encode:(fun (src : string) pointer_bytes ->
        (* Note: could avoid an allocation if the new string is not longer
           than the old one.  This deep copy strategy has the advantage of
           being correct in all cases. *)
        let new_string_storage = uint8_list_of_string
            ~null_terminated:false ~dest_message:pointer_bytes.Slice.msg
            src
        in
        let () = deep_zero_pointer pointer_bytes in
        init_list_pointer pointer_bytes new_string_storage)


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto List<Text>. *)
  let get_struct_field_text_list
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
    : (string, 'b) Runtime.BArray.t =
    get_struct_field_bytes_list struct_storage pointer_word
      ~element_size:8
      ~decode:(fun slice ->
        let pointer_bytes = get_struct_pointer struct_storage pointer_word in
        (* Text fields are always accessed by value, not by reference, since
           we always do an immediate decode to [string].  Therefore we can
           use the Reader logic to handle this case. *)
        match Reader.deref_list_pointer pointer_bytes with
        | Some list_storage ->
            string_of_uint8_list ~null_terminated:true list_storage
        | None ->
            "")
      ~encode:(fun (src : string) pointer_bytes ->
        (* Note: could avoid an allocation if the new string is not longer
           than the old one.  This deep copy strategy has the advantage of
           being correct in all cases. *)
        let new_string_storage = uint8_list_of_string
            ~null_terminated:true ~dest_message:pointer_bytes.Slice.msg
            src
        in
        let () = deep_zero_pointer pointer_bytes in
        init_list_pointer pointer_bytes new_string_storage)


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto List<S> for some struct S.  The [data_words] and
     [pointer_words] describe the struct layout. *)
  let get_struct_field_struct_list
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      ~(data_words : int)
      ~(pointer_words : int)
    : ('a, 'b) Runtime.BArray.t =
    let pointer_bytes = get_struct_pointer struct_storage pointer_word in
    let alloc_default_list message =
      alloc_list_storage message (ListStorage.Composite (data_words, pointer_words)) 0
    in
    let list_storage = deref_list_pointer pointer_bytes alloc_default_list in
    Runtime.BArray.make
      ~length:(fun x -> x.ListStorage.num_elements)
      ~get:StructList.get
      ~set:StructList.set
      list_storage


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto List<L> for some list L. *)
  let get_struct_field_list_list
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
    : (rw ListStorage.t, 'b) Runtime.BArray.t =
    get_struct_field_bytes_list struct_storage pointer_word
      ~element_size:8
      ~decode:(fun slice ->
        let alloc_default_list message =
          alloc_list_storage message ListStorage.Pointer 0
        in
        deref_list_pointer slice alloc_default_list)
      ~encode:(fun (src_list_storage : 'cap ListStorage.t) pointer_bytes ->
        let new_list_storage = deep_copy_list ~src:src_list_storage
            ~dest_message:pointer_bytes.Slice.msg
        in
        let () = deep_zero_pointer pointer_bytes in
        init_list_pointer pointer_bytes new_list_storage)


  (* Given storage for a struct, get the data for the specified
     struct-embedded pointer under the assumption that it points to a
     cap'n proto List<E> for some enum E. *)
  let get_struct_field_enum_list
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      ~(decode : int -> 'a)
      ~(encode : 'a -> int)
    : ('a, 'b) Runtime.BArray.t =
    get_struct_field_bytes_list struct_storage pointer_word
      ~element_size:2
      ~decode:(fun slice -> decode (Slice.get_uint16 slice 0))
      ~encode:(fun v slice -> Slice.set_uint16 slice 0 (encode v))


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
      (byte_ofs : int) (bit_ofs : int)
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


  (* Given storage for a struct, set the data for the specified struct-embedded
     pointer under the assumption that it points to a cap'n proto Data payload. *)
  let set_struct_field_blob
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
    init_list_pointer pointer_bytes new_string_storage


  (* Given storage for a struct, set the data for the specified struct-embedded
     pointer under the assumption that it points to a cap'n proto Text payload. *)
  let set_struct_field_text
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
    init_list_pointer pointer_bytes new_string_storage


  (* Given storage for a struct, set the data for the specified struct-embedded
     pointer equal to a deep copy of the specified [src] list. *)
  let set_struct_field_list
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      (src : 'cap ListStorage.t)
    : unit =
    let pointer_bytes = get_struct_pointer struct_storage pointer_word in
    let list_storage = deep_copy_list ~src
        ~dest_message:struct_storage.StructStorage.pointers.Slice.msg
    in
    let () = deep_zero_pointer pointer_bytes in
    init_list_pointer pointer_bytes list_storage


  (* Given storage for a struct, set the data for the specified struct-embedded
     pointer equal to a deep copy of the specified [src] struct. *)
  let set_struct_field_struct
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      (src : 'cap StructStorage.t)
    : unit =
    let pointer_bytes = get_struct_pointer struct_storage pointer_word in
    let new_storage = deep_copy_struct ~src
        ~dest_message:struct_storage.StructStorage.pointers.Slice.msg
    in
    let () = deep_zero_pointer pointer_bytes in
    init_struct_pointer pointer_bytes new_storage


  (* Given storage for a struct, write a new value for the boolean field stored
     at the given byte and bit offset within the struct's data region. *)
  let set_struct_field_bit
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
    Slice.set_uint8 struct_storage.StructStorage.data byte_ofs byte_val


  (* Given storage for a struct, write a new value for the UInt8 field stored
     at the given byte offset within the struct's data region. *)
  let set_struct_field_uint8
      (struct_storage : rw StructStorage.t)
      ~(default : int)
      (byte_ofs : int)
      (value : int)
    : unit =
    Slice.set_uint8 struct_storage.StructStorage.data byte_ofs
      (value lxor default)


  (* Given storage for a struct, write a new value for the UInt16 field stored
     at the given byte offset within the struct's data region. *)
  let set_struct_field_uint16
      (struct_storage : rw StructStorage.t)
      ~(default : int)
      (byte_ofs : int)
      (value : int)
    : unit =
    Slice.set_uint16 struct_storage.StructStorage.data byte_ofs
      (value lxor default)


  (* Given storage for a struct, write a new value for the UInt32 field stored
     at the given byte offset within the struct's data region. *)
  let set_struct_field_uint32
      (struct_storage : rw StructStorage.t)
      ~(default : Uint32.t)
      (byte_ofs : int)
      (value : Uint32.t)
    : unit =
    Slice.set_uint32 struct_storage.StructStorage.data byte_ofs
      (Uint32.logxor value default)


  (* Given storage for a struct, write a new value for the UInt64 field stored
     at the given byte offset within the struct's data region. *)
  let set_struct_field_uint64
      (struct_storage : rw StructStorage.t)
      ~(default : Uint64.t)
      (byte_ofs : int)
      (value : Uint64.t)
    : unit =
    Slice.set_uint64 struct_storage.StructStorage.data byte_ofs
      (Uint64.logxor value default)


  (* Given storage for a struct, write a new value for the Int8 field stored
     at the given byte offset within the struct's data region. *)
  let set_struct_field_int8
      (struct_storage : rw StructStorage.t)
      ~(default : int)
      (byte_ofs : int)
      (value : int)
    : unit =
    Slice.set_int8 struct_storage.StructStorage.data byte_ofs
      (value lxor default)


  (* Given storage for a struct, write a new value for the Int16 field stored
     at the given byte offset within the struct's data region. *)
  let set_struct_field_int16
      (struct_storage : rw StructStorage.t)
      ~(default : int)
      (byte_ofs : int)
      (value : int)
    : unit =
    Slice.set_int16 struct_storage.StructStorage.data byte_ofs
      (value lxor default)


  (* Given storage for a struct, write a new value for the Int32 field stored
     at the given byte offset within the struct's data region. *)
  let set_struct_field_int32
      (struct_storage : rw StructStorage.t)
      ~(default : int32)
      (byte_ofs : int)
      (value : int32)
    : unit =
    Slice.set_int32 struct_storage.StructStorage.data byte_ofs
      (Int32.bit_xor value default)


  (* Given storage for a struct, write a new value for the Int64 field stored
     at the given byte offset within the struct's data region. *)
  let set_struct_field_int64
      (struct_storage : rw StructStorage.t)
      ~(default : int64)
      (byte_ofs : int)
      (value : int64)
    : unit =
    Slice.set_int64 struct_storage.StructStorage.data byte_ofs
      (Int64.bit_xor value default)


  (* Given storage for a struct, allocate a new List<T> member for one of the
     pointers stored within the struct. *)
  let init_struct_field_list
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
      init_list_pointer pointer_bytes list_storage


  (* Given storage for a struct, allocate a new List<Bool> member for one
     of the pointers stored within the struct.  Returns an array builder
     for the resulting list. *)
  let init_struct_field_bit_list
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      ~(num_elements : int)
    : ('a, 'b) Runtime.BArray.t =
    let () = init_struct_field_list struct_storage pointer_word
        ~storage_type:ListStorage.Bit ~num_elements
    in
    get_struct_field_bit_list struct_storage pointer_word


  (* Given storage for a struct, allocate a new List<Int8> member for
     one of the pointers stored within the struct.  Returns an array
     builder for the resulting list. *)
  let init_struct_field_int8_list
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      ~(num_elements : int)
    : (int, 'b) Runtime.BArray.t =
    let () = init_struct_field_list struct_storage pointer_word
        ~storage_type:(ListStorage.Bytes 1) ~num_elements
    in
    get_struct_field_int8_list struct_storage pointer_word


  (* Given storage for a struct, allocate a new List<Int16> member for
     one of the pointers stored within the struct.  Returns an array
     builder for the resulting list. *)
  let init_struct_field_int16_list
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      ~(num_elements : int)
    : (int, 'b) Runtime.BArray.t =
    let () = init_struct_field_list struct_storage pointer_word
        ~storage_type:(ListStorage.Bytes 2) ~num_elements
    in
    get_struct_field_int16_list struct_storage pointer_word


  (* Given storage for a struct, allocate a new List<Int32> member for
     one of the pointers stored within the struct.  Returns an array
     builder for the resulting list. *)
  let init_struct_field_int32_list
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      ~(num_elements : int)
    : (int32, 'b) Runtime.BArray.t =
    let () = init_struct_field_list struct_storage pointer_word
        ~storage_type:(ListStorage.Bytes 4) ~num_elements
    in
    get_struct_field_int32_list struct_storage pointer_word


  (* Given storage for a struct, allocate a new List<Int64> member for
     one of the pointers stored within the struct.  Returns an array
     builder for the resulting list. *)
  let init_struct_field_int64_list
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      ~(num_elements : int)
    : (int64, 'b) Runtime.BArray.t =
    let () = init_struct_field_list struct_storage pointer_word
        ~storage_type:(ListStorage.Bytes 8) ~num_elements
    in
    get_struct_field_int64_list struct_storage pointer_word


  (* Given storage for a struct, allocate a new List<UInt8> member for
     one of the pointers stored within the struct.  Returns an array
     builder for the resulting list. *)
  let init_struct_field_uint8_list
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      ~(num_elements : int)
    : (int, 'b) Runtime.BArray.t =
    let () = init_struct_field_list struct_storage pointer_word
        ~storage_type:(ListStorage.Bytes 1) ~num_elements
    in
    get_struct_field_uint8_list struct_storage pointer_word


  (* Given storage for a struct, allocate a new List<UInt16> member for
     one of the pointers stored within the struct.  Returns an array
     builder for the resulting list. *)
  let init_struct_field_uint16_list
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      ~(num_elements : int)
    : (int, 'b) Runtime.BArray.t =
    let () = init_struct_field_list struct_storage pointer_word
        ~storage_type:(ListStorage.Bytes 2) ~num_elements
    in
    get_struct_field_uint16_list struct_storage pointer_word


  (* Given storage for a struct, allocate a new List<UInt32> member for
     one of the pointers stored within the struct.  Returns an array
     builder for the resulting list. *)
  let init_struct_field_uint32_list
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      ~(num_elements : int)
    : (Uint32.t, 'b) Runtime.BArray.t =
    let () = init_struct_field_list struct_storage pointer_word
        ~storage_type:(ListStorage.Bytes 4) ~num_elements
    in
    get_struct_field_uint32_list struct_storage pointer_word


  (* Given storage for a struct, allocate a new List<UInt64> member for
     one of the pointers stored within the struct.  Returns an array
     builder for the resulting list. *)
  let get_struct_field_uint64_list
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      ~(num_elements : int)
    : (Uint64.t, 'b) Runtime.BArray.t =
    let () = init_struct_field_list struct_storage pointer_word
        ~storage_type:(ListStorage.Bytes 8) ~num_elements
    in
    get_struct_field_uint64_list struct_storage pointer_word


  (* Given storage for a struct, allocate a new List<Float32> member for
     one of the pointers stored within the struct.  Returns an array
     builder for the resulting list. *)
  let init_struct_field_float32_list
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      ~(num_elements : int)
    : (float, 'b) Runtime.BArray.t =
    let () = init_struct_field_list struct_storage pointer_word
        ~storage_type:(ListStorage.Bytes 4) ~num_elements
    in
    get_struct_field_float32_list struct_storage pointer_word


  (* Given storage for a struct, allocate a new List<Float64> member for
     one of the pointers stored within the struct.  Returns an array
     builder for the resulting list. *)
  let init_struct_field_float64_list
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      ~(num_elements : int)
    : (float, 'b) Runtime.BArray.t =
    let () = init_struct_field_list struct_storage pointer_word
        ~storage_type:(ListStorage.Bytes 8) ~num_elements
    in
    get_struct_field_float64_list struct_storage pointer_word


  (* Given storage for a struct, allocate a new List<List<Data>> member for
     one of the pointers stored within the struct.  Returns an array
     builder for the resulting list. *)
  let init_struct_field_blob_list
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      ~(num_elements : int)
    : (string, 'b) Runtime.BArray.t =
    let () = init_struct_field_list struct_storage pointer_word
        ~storage_type:ListStorage.Pointer ~num_elements
    in
    get_struct_field_blob_list struct_storage pointer_word


  (* Given storage for a struct, allocate a new List<List<Text>> member for
     one of the pointers stored within the struct.  Returns an array
     builder for the resulting list. *)
  let init_struct_field_text_list
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      ~(num_elements : int)
    : (string, 'b) Runtime.BArray.t =
    let () = init_struct_field_list struct_storage pointer_word
        ~storage_type:ListStorage.Pointer ~num_elements
    in
    get_struct_field_text_list struct_storage pointer_word


  (* Given storage for a struct, allocate a new List<T> member for one of the
     pointers stored within the struct, where T is a struct type.  Returns
     an array builder for the resulting list. *)
  let init_struct_field_struct_list
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      ~(num_elements : int)
      ~(data_words : int)
      ~(pointer_words : int)
    : ('a, 'b) Runtime.BArray.t =
    let () = init_struct_field_list struct_storage pointer_word
        ~storage_type:(ListStorage.Composite (data_words, pointer_words))
        ~num_elements
    in
    get_struct_field_struct_list struct_storage pointer_word
      ~data_words ~pointer_words


  (* Given storage for a struct, allocate a new List<List<T>> member for
     one of the pointers stored within the struct.  Returns an array builder
     for the resulting list. *)
  let init_struct_field_list_list
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      ~(num_elements : int)
    : (rw ListStorage.t, 'b) Runtime.BArray.t =
    let () = init_struct_field_list struct_storage pointer_word
        ~storage_type:ListStorage.Pointer ~num_elements
    in
    get_struct_field_list_list struct_storage pointer_word


  (* Given storage for a struct, allocate a new List<Enum> member for
     one of the pointers stored within the struct.  Returns an array builder
     for the resulting list. *)
  let init_struct_field_enum_list
      (struct_storage : rw StructStorage.t)
      (pointer_word : int)
      ~(num_elements : int)
      ~(decode : int -> 'a)
      ~(encode : 'a -> int)
    : ('a, 'b) Runtime.BArray.t =
    let () = init_struct_field_list struct_storage pointer_word
        ~storage_type:(ListStorage.Bytes 2) ~num_elements
    in
    get_struct_field_enum_list struct_storage pointer_word
      ~decode ~encode


  (* Given storage for a parent struct, allocate a new child struct and link it
     to one of the pointers stored within the parent.  Returns a reference to
     the newly-created struct. *)
  let init_struct_field_struct
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


