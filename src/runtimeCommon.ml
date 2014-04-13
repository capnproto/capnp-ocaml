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

module Make (MessageWrapper : Message.S) = struct
  let invalid_msg = Message.invalid_msg
  type ro = Message.ro
  type rw = Message.rw
  include MessageWrapper


  let sizeof_uint32 = 4
  let sizeof_uint64 = 8


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


  module StructStorage = struct
    (** Storage associated with a cap'n proto struct. *)
    type 'cap t = {
      data     : 'cap Slice.t;  (** Storage for struct fields stored by value *)
      pointers : 'cap Slice.t;  (** Storage for struct fields stored by reference *)
    }

    (** Get the range of bytes associated with a pointer stored in a struct. *)
    let get_pointer
        (struct_storage : 'cap t)
        (word : int)           (* Struct-relative pointer index *)
      : 'cap Slice.t option =  (* Returns None if storage is too small for this word *)
      let pointers = struct_storage.pointers in
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

  end


  module ListStorage = struct
    type storage_type_t =
      (** list(void), no storage required *)
      | Empty

      (** list(bool), tightly packed bits *)
      | Bit

      (** either primitive values or a data-only struct; argument is the byte
          count *)
      | Bytes of int

      (** either a pointer to an external object, or a pointer-only struct *)
      | Pointer

      (** typical struct; parameters are per-element word size for data section
          and pointers section, respectively *)
      | Composite of int * int

    (** Storage associated with a cap'n proto list. *)
    type 'cap t = {
      storage      : 'cap Slice.t;      (** Range of bytes used to hold list elements *)
      storage_type : storage_type_t;    (** Describes the list packing format *)
      num_elements : int;               (** Number of list elements *)
    }
  end


  module Object = struct
    type 'cap t =
      | None
      | List of 'cap ListStorage.t
      | Struct of 'cap StructStorage.t
  end


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
    let make_list_storage_aux ~offset ~num_words ~num_elements ~storage_type =
      let storage = {
        Slice.msg        = message;
        Slice.segment_id = segment_id;
        Slice.start      = segment_offset + offset;
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
        make_list_storage_aux ~offset:0 ~num_words:0
          ~num_elements:list_pointer.num_elements ~storage_type:ListStorage.Empty
    | OneBitValue ->
        make_list_storage_aux ~offset:0
          ~num_words:(Util.ceil_int list_pointer.num_elements 64)
          ~num_elements:list_pointer.num_elements ~storage_type:ListStorage.Bit
    | OneByteValue ->
        make_list_storage_aux ~offset:0
          ~num_words:(Util.ceil_int list_pointer.num_elements 8)
          ~num_elements:list_pointer.num_elements
          ~storage_type:(ListStorage.Bytes 1)
    | TwoByteValue ->
        make_list_storage_aux ~offset:0
          ~num_words:(Util.ceil_int list_pointer.num_elements 4)
          ~num_elements:list_pointer.num_elements
          ~storage_type:(ListStorage.Bytes 2)
    | FourByteValue ->
        make_list_storage_aux ~offset:0
          ~num_words:(Util.ceil_int list_pointer.num_elements 2)
          ~num_elements:list_pointer.num_elements
          ~storage_type:(ListStorage.Bytes 4)
    | EightByteValue ->
        make_list_storage_aux ~offset:0 ~num_words:list_pointer.num_elements
          ~num_elements:list_pointer.num_elements
          ~storage_type:(ListStorage.Bytes 8)
    | EightBytePointer ->
        make_list_storage_aux ~offset:0 ~num_words:list_pointer.num_elements
          ~num_elements:list_pointer.num_elements
          ~storage_type:ListStorage.Pointer
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
              invalid_msg "composite list pointer describes invalid word count";
            make_list_storage_aux ~offset:sizeof_uint64
              ~num_words:list_pointer.num_elements
              ~num_elements:struct_pointer.SP.offset
              ~storage_type:(ListStorage.Composite
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


  let list_fold_left_generic
      (list_storage : 'cap ListStorage.t)
      ~(get : 'cap ListStorage.t -> int -> 'a)
      ~(f : 'b -> 'a -> 'b)
      ~(init : 'b)
    : 'b =
    let rec loop acc i =
      if i = list_storage.ListStorage.num_elements then
        acc
      else
        let v = get list_storage i in
        loop (f acc v) (i + 1)
    in
    loop init 0


  let list_fold_right_generic
      (list_storage : 'cap ListStorage.t)
      ~(get : 'cap ListStorage.t -> int -> 'a)
      ~(f : 'b -> 'a -> 'b)
      ~(init : 'b)
    : 'b =
    let rec loop acc i =
      if i < 0 then
        acc
      else
        let v = get list_storage i in
        loop (f acc v) (i - 1)
    in
    loop init (list_storage.ListStorage.num_elements - 1)


  (* FIXME: rewrite as make_get() and make_set() which perform pointer decoding
     up-front and return a closure *)
  module BitList = struct
    let get (list_storage : 'cap ListStorage.t) (i : int) : bool =
      match list_storage.ListStorage.storage_type with
      | ListStorage.Bit ->
          let byte_ofs = i / 8 in
          let bit_ofs  = i mod 8 in
          let byte_val =
            Slice.get_uint8 list_storage.ListStorage.storage byte_ofs
          in
          (byte_val land (1 lsl bit_ofs)) <> 0
      | _ ->
          invalid_msg "decoded non-bool list where bool list was expected"

    let set (list_storage : rw ListStorage.t) (i : int) (v : bool) : unit =
      match list_storage.ListStorage.storage_type with
      | ListStorage.Bit ->
          let byte_ofs = i / 8 in
          let bit_ofs  = i mod 8 in
          let bitmask  = 1 lsl bit_ofs in
          let old_byte_val =
            Slice.get_uint8 list_storage.ListStorage.storage byte_ofs
          in
          let new_byte_val =
            if v then
              old_byte_val lor bitmask
            else
              old_byte_val land (lnot bitmask)
          in
          Slice.set_uint8 list_storage.ListStorage.storage byte_ofs new_byte_val
      | _ ->
          invalid_msg "decoded non-bool list where bool list was expected"

    let fold_left
        (list_storage : 'cap ListStorage.t)
        ~(f : 'a -> bool -> 'a)
        ~(init : 'a)
      : 'a =
      list_fold_left_generic list_storage ~get ~f ~init

    let fold_right
        (list_storage : 'cap ListStorage.t)
        ~(f : 'a -> bool -> 'a)
        ~(init : 'a)
      : 'a =
      list_fold_right_generic list_storage ~get ~f ~init
  end


  (* FIXME: rewrite as make_get() and make_set() which perform pointer decoding
     up-front and return a closure *)
  module BytesList = struct
    let get (list_storage : 'cap ListStorage.t) (i : int) : 'cap Slice.t =
      let byte_count =
        match list_storage.ListStorage.storage_type with
        | ListStorage.Bytes byte_count -> byte_count
        | ListStorage.Pointer          -> sizeof_uint64
        | _ -> invalid_msg "decoded non-bytes list where bytes list was expected"
      in {
        list_storage.ListStorage.storage with
        Slice.start =
          list_storage.ListStorage.storage.Slice.start + (i * byte_count);
        Slice.len = byte_count
      }

    let fold_left
        (list_storage : 'cap ListStorage.t)
        ~(f : 'a -> 'cap Slice.t -> 'a)
        ~(init : 'a)
      : 'a =
      list_fold_left_generic list_storage ~get ~f ~init

    let fold_right
        (list_storage : 'cap ListStorage.t)
        ~(f : 'a -> 'cap Slice.t -> 'a)
        ~(init : 'a)
      : 'a =
      list_fold_right_generic list_storage ~get ~f ~init
  end


  (* FIXME: rewrite as make_get() and make_set() which perform pointer decoding
     up-front and return a closure *)
  module StructList = struct
    let get (list_storage : 'cap ListStorage.t) (i : int) : 'cap StructStorage.t =
      match list_storage.ListStorage.storage_type with
      | ListStorage.Bytes byte_count ->
          (* List storage contains inlined data-only structs *)
          let data = {
            list_storage.ListStorage.storage with
            Slice.start =
              list_storage.ListStorage.storage.Slice.start + (i * byte_count);
            Slice.len = byte_count;
          } in
          let pointers = {
            list_storage.ListStorage.storage with
            Slice.start = 0;
            Slice.len   = 0;
          } in
          { StructStorage.data; StructStorage.pointers; }
      | ListStorage.Pointer ->
          (* List storage contains inlined single-pointer-only structs *)
          let data = {
            list_storage.ListStorage.storage with
            Slice.start = 0;
            Slice.len   = 0;
          } in
          let pointers = {
            list_storage.ListStorage.storage with
            Slice.start =
              list_storage.ListStorage.storage.Slice.start + (i * sizeof_uint64);
            Slice.len = sizeof_uint64;
          } in
          { StructStorage.data; StructStorage.pointers; }
      | ListStorage.Composite (data_words, pointers_words) ->
          (* List storage contains generic inlined structs *)
          let data_size     = data_words * sizeof_uint64 in
          let pointers_size = pointers_words * sizeof_uint64 in
          let total_size    = data_size + pointers_size in
          let data = {
            list_storage.ListStorage.storage with
            Slice.start =
              list_storage.ListStorage.storage.Slice.start + (i * total_size);
            Slice.len = data_size;
          } in
          let pointers = {
            data with
            Slice.start = Slice.get_end data;
            Slice.len   = pointers_size;
          } in
          { StructStorage.data; StructStorage.pointers; }
      | ListStorage.Empty | ListStorage.Bit ->
          invalid_msg "decoded non-struct list where struct list was expected"


    (* FIXME: This needs to make a deep copy *)
    let set (list_storage : 'cap ListStorage.t)
        (i : int)
        (v : 'cap StructStorage.t)
      : unit =
      failwith "not implemented"


    let fold_left
        (list_storage : 'cap ListStorage.t)
        ~(f : 'a -> 'cap StructStorage.t -> 'a)
        ~(init : 'a)
      : 'a =
      list_fold_left_generic list_storage ~get ~f ~init


    let fold_right
        (list_storage : 'cap ListStorage.t)
        ~(f : 'a -> 'cap StructStorage.t -> 'a)
        ~(init : 'a)
      : 'a =
      list_fold_right_generic list_storage ~get ~f ~init
  end


  (* Given list storage which is expected to contain UInt8 data, decode the data as
     an OCaml string. *)
  let string_of_uint8_list
      ~(null_terminated : bool)   (* true if the data is expected to end in 0 *)
      (list_storage : 'cap ListStorage.t)
    : string =
    let open ListStorage in
    match list_storage.storage_type with
    | Bytes 1 ->
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
        let buf = String.create result_byte_count in
        let () =
          for i = 0 to result_byte_count - 1 do
            buf.[i] <- Char.of_int_exn (Slice.get_uint8 list_storage.storage i)
          done
        in
        buf
    | _ ->
        invalid_msg "decoded non-UInt8 list where string data was expected"

end
