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


type ro = MessageSig.ro
type rw = MessageSig.rw

module Make (Storage : MessageStorage.S) = struct
  module Segment = struct
    type storage_t = Storage.t
    type -'cap t = Storage.t

    let alloc size   = Storage.alloc (Util.round_up_mult_8 size)
    let release      = Storage.release
    let length       = Storage.length
    let readonly s   = s
    let of_storage s = s
    let to_storage s = s

    let get_uint8        = Storage.get_uint8
    let get_uint16       = Storage.get_uint16
    let get_uint32       = Storage.get_uint32
    let get_uint64       = Storage.get_uint64
    let get_int8         = Storage.get_int8
    let get_int16        = Storage.get_int16
    let get_int32        = Storage.get_int32
    let get_int64        = Storage.get_int64
    let set_uint8        = Storage.set_uint8
    let set_uint16       = Storage.set_uint16
    let set_uint32       = Storage.set_uint32
    let set_uint64       = Storage.set_uint64
    let set_int8         = Storage.set_int8
    let set_int16        = Storage.set_int16
    let set_int32        = Storage.set_int32
    let set_int64        = Storage.set_int64
    let blit             = Storage.blit
    let blit_to_bytes    = Storage.blit_to_bytes
    let blit_from_string = Storage.blit_from_string
    let zero_out         = Storage.zero_out
  end   (* module Segment *)

  module Message = struct
    type storage_t = Storage.t

    type storage_descr_t = {
      segment : storage_t;
      bytes_consumed : int;
    }

    type -'cap t = {
      segments : storage_descr_t Res.Array.t;
      attachments : MessageSig.attachments;
    }

    let create size =
      let segment = Storage.alloc (Util.round_up_mult_8 size) in
      let segments = Res.Array.create 1 {segment; bytes_consumed = 0} in
      { segments; attachments = MessageSig.No_attachments }

    let release m =
      let () = Res.Array.iter (fun descr -> Storage.release descr.segment) m.segments in
      Res.Array.clear m.segments

    let num_segments m = Res.Array.length m.segments

    let total_size m =
      Res.Array.fold_left (fun acc x -> acc + x.bytes_consumed) 0 m.segments

    let total_alloc_size m =
      Res.Array.fold_left (fun acc x -> acc + (Segment.length x.segment)) 0 m.segments

    let get_segment m i = (Res.Array.get m.segments i).segment

    let get_segment_descr m i = Res.Array.get m.segments i

    let add_segment m new_seg =
      let new_seg_descr = {segment = new_seg; bytes_consumed = 0} in
      Res.Array.add_one m.segments new_seg_descr;
      new_seg_descr

    let readonly m = (m :> ro t)

    let of_storage ms =
      let segments = Res.Array.empty () in
      let () = ListLabels.iter ms ~f:(fun x ->
          Res.Array.add_one segments {segment = x; bytes_consumed = Storage.length x}) in
      { segments; attachments = MessageSig.No_attachments }

    let to_storage m = Res.Array.fold_right (fun x acc -> x :: acc) m.segments []

    let with_message m ~f = Core_kernel.Exn.protectx m ~f ~finally:release

    let with_attachments attachments m = { m with attachments }
    let get_attachments m = m.attachments
  end   (* module Message *)

  module Slice = struct
    type -'cap segment_t = Storage.t
    type -'cap message_t = 'cap Message.t

    type 'cap t = {
      msg        : 'cap message_t;
      segment    : 'cap segment_t;
      segment_id : int;
      start      : int;
      len        : int;
    }

    let alloc m size =
      (* Going with a wasteful algorithm for now: allocations only happen on the
         last segment, with no attempt to go back and do best-fit on previous
         segments.  This wastes memory, but unallocated regions will pack
         very efficiently. *)
      let size = Util.round_up_mult_8 size in
      let segment_id, segment_descr =
        let last_seg_id = Message.num_segments m - 1 in
        let last_seg_descr = Message.get_segment_descr m last_seg_id in
        let bytes_avail = Storage.length last_seg_descr.Message.segment -
          last_seg_descr.Message.bytes_consumed
        in
        if size <= bytes_avail then
          last_seg_id, last_seg_descr
        else
          (* Doubling message size on each allocation, under the assumption
             that message sizes will be exponentially distributed. *)
          let min_alloc_size = Message.total_alloc_size m in
          let new_seg = Storage.alloc (max min_alloc_size size) in
          let new_seg_descr = Message.add_segment m new_seg in
          last_seg_id + 1, new_seg_descr
      in
      let slice = {
        msg = m;
        segment = segment_descr.Message.segment;
        segment_id;
        start = segment_descr.Message.bytes_consumed;
        len = size;
      } in
      let () = Res.Array.set m.segments segment_id
        { segment_descr with
          Message.bytes_consumed = slice.start + slice.len }
      in
      (* Allocations should be eight-byte aligned *)
      let () = assert ((slice.start land 7) = 0) in
      slice

    let alloc_in_segment m segment_id size =
      let size = Util.round_up_mult_8 size in
      let seg_descr = Message.get_segment_descr m segment_id in
      let bytes_avail = Storage.length seg_descr.Message.segment -
          seg_descr.Message.bytes_consumed
      in
      if size <= bytes_avail then
        let slice = {
          msg = m;
          segment = seg_descr.Message.segment;
          segment_id;
          start = seg_descr.Message.bytes_consumed;
          len = size;
        } in
        let () = Res.Array.set m.segments segment_id
          { seg_descr with
            Message.bytes_consumed = slice.start + slice.len }
        in
        (* Allocations should be eight-byte aligned *)
        let () = assert ((slice.start land 7) = 0) in
        Some slice
      else
        None

    let get_segment slice = slice.segment

    let get_end slice = slice.start + slice.len

    let readonly slice = {
      slice with
      msg = Message.readonly slice.msg;
    }

    let get_uint8 slice i =
      if i < 0 || i > slice.len - 1 then
        invalid_arg "Slice.get_uint8"
      else
        let segment = get_segment slice in
        Segment.get_uint8 segment (slice.start + i)

    let get_uint16 slice i =
      if i < 0 || i > slice.len - 2 then
        invalid_arg "Slice.get_uint16"
      else
        let segment = get_segment slice in
        Segment.get_uint16 segment (slice.start + i)

    let get_uint32 slice i =
      if i < 0 || i > slice.len - 4 then
        invalid_arg "Slice.get_uint32"
      else
        let segment = get_segment slice in
        Segment.get_uint32 segment (slice.start + i)

    let get_uint64 slice i =
      if i < 0 || i > slice.len - 8 then
        invalid_arg "Slice.get_uint64"
      else
        let segment = get_segment slice in
        Segment.get_uint64 segment (slice.start + i)

    let get_int8 slice i =
      if i < 0 || i > slice.len - 1 then
        invalid_arg "Slice.get_int8"
      else
        let segment = get_segment slice in
        Segment.get_int8 segment (slice.start + i)

    let get_int16 slice i =
      if i < 0 || i > slice.len - 2 then
        invalid_arg "Slice.get_int16"
      else
        let segment = get_segment slice in
        Segment.get_int16 segment (slice.start + i)

    let get_int32 slice i =
      if i < 0 || i > slice.len - 4 then
        invalid_arg "Slice.get_int32"
      else
        let segment = get_segment slice in
        Segment.get_int32 segment (slice.start + i)

    let get_int64 slice i =
      if i < 0 || i > slice.len - 8 then
        invalid_arg "Slice.get_int64"
      else
        let segment = get_segment slice in
        Segment.get_int64 segment (slice.start + i)

    let set_uint8 slice i v =
      if i < 0 || i > slice.len - 1 then
        invalid_arg "Slice.set_uint8"
      else
        let segment = get_segment slice in
        Segment.set_uint8 segment (slice.start + i) v

    let set_uint16 slice i v =
      if i < 0 || i > slice.len - 2 then
        invalid_arg "Slice.set_uint16"
      else
        let segment = get_segment slice in
        Segment.set_uint16 segment (slice.start + i) v

    let set_uint32 slice i v =
      if i < 0 || i > slice.len - 4 then
        invalid_arg "Slice.set_uint32"
      else
        let segment = get_segment slice in
        Segment.set_uint32 segment (slice.start + i) v

    let set_uint64 slice i v =
      if i < 0 || i > slice.len - 8 then
        invalid_arg "Slice.set_uint64"
      else
        let segment = get_segment slice in
        Segment.set_uint64 segment (slice.start + i) v

    let set_int8 slice i v =
      if i < 0 || i > slice.len - 1 then
        invalid_arg "Slice.set_int8"
      else
        let segment = get_segment slice in
        Segment.set_int8 segment (slice.start + i) v

    let set_int16 slice i v =
      if i < 0 || i > slice.len - 2 then
        invalid_arg "Slice.set_int16"
      else
        let segment = get_segment slice in
        Segment.set_int16 segment (slice.start + i) v

    let set_int32 slice i v =
      if i < 0 || i > slice.len - 4 then
        invalid_arg "Slice.set_int32"
      else
        let segment = get_segment slice in
        Segment.set_int32 segment (slice.start + i) v

    let set_int64 slice i v =
      if i < 0 || i > slice.len - 8 then
        invalid_arg "Slice.set_int64"
      else
        let segment = get_segment slice in
        Segment.set_int64 segment (slice.start + i) v

    let blit ~src ~src_pos ~dst ~dst_pos ~len =
      if src_pos < 0 || src_pos + len > src.len ||
         dst_pos < 0 || dst_pos + len > dst.len then
        invalid_arg "Slice.blit"
      else
        let src_seg = get_segment src in
        let dst_seg = get_segment dst in
        Segment.blit
          ~src:src_seg ~src_pos:(src.start + src_pos)
          ~dst:dst_seg ~dst_pos:(dst.start + dst_pos)
          ~len

    let blit_to_bytes ~src ~src_pos ~dst ~dst_pos ~len =
      if src_pos < 0 || src_pos + len > src.len then
        invalid_arg "Slice.blit_to_bytes"
      else
        Segment.blit_to_bytes
          ~src:(get_segment src) ~src_pos:(src.start + src_pos)
          ~dst ~dst_pos ~len

    let blit_from_string ~src ~src_pos ~dst ~dst_pos ~len =
      if dst_pos < 0 || dst_pos + len > dst.len then
        invalid_arg "Slice.blit_from_string"
      else
        Segment.blit_from_string
          ~src ~src_pos
          ~dst:(get_segment dst) ~dst_pos:(dst.start + dst_pos)
          ~len

    let zero_out slice ~pos ~len =
      if pos < 0 || pos + len > slice.len then
        invalid_arg "Slice.zero_out"
      else
        Segment.zero_out ~pos:(slice.start + pos) ~len (get_segment slice)

  end   (* module Slice *)

  module StructStorage = struct
    (** Storage associated with a cap'n proto struct. *)
    type ('cap, 'a) t = {
      data     : 'cap Slice.t;  (** Storage for struct fields stored by value *)
      pointers : 'cap Slice.t;  (** Storage for struct fields stored by reference *)
    }

    let readonly (struct_storage : ('cap, 'a) t) : (ro, 'a) t = {
      data     = Slice.readonly struct_storage.data;
      pointers = Slice.readonly struct_storage.pointers;
    }

    let with_attachments (a : MessageSig.attachments) (struct_storage : ('cap, 'a) t) =
      let msg = Message.with_attachments a struct_storage.data.Slice.msg in
      {
        data     = { struct_storage.data with Slice.msg };
        pointers = { struct_storage.pointers with Slice.msg };
      }

    let get_attachments (struct_storage : ('cap, 'a) t) =
      Message.get_attachments struct_storage.data.Slice.msg

    let v ~data ~pointers = { data; pointers }

    let cast x = (x :> ('cap, 'a) t)

    type 'a reader_t = (ro, 'a) t option
    type 'a builder_t = (rw, 'a) t

    let cast_reader x = (x :> 'a reader_t)

    let reader_of_builder x = Some (readonly x)
    let message_of_builder x = x.data.Slice.msg
  end

  module ListStorage = struct
    (** Storage associated with a cap'n proto list. *)
    type 'cap t = {
      storage      : 'cap Slice.t;      (** Range of bytes used to hold list elements *)
      storage_type : ListStorageType.t; (** Describes the list packing format *)
      num_elements : int;               (** Number of list elements *)
    }

    let readonly (list_storage : 'cap t) : ro t = {
      storage      = Slice.readonly list_storage.storage;
      storage_type = list_storage.storage_type;
      num_elements = list_storage.num_elements;
    }
  end

  module Object = struct
    type ('cap, 'a) t =
      | None
      | List of 'cap ListStorage.t
      | Struct of ('cap, 'a) StructStorage.t
      | Capability of Uint32.t
  end

end [@@inline]

module BytesMessage = Make[@inlined](BytesStorage)


exception Invalid_message of string
exception Out_of_int_range = Util.Out_of_int_range
let invalid_msg s = raise (Invalid_message s)
let out_of_int_range s = raise (Out_of_int_range s)


