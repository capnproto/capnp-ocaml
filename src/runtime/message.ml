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


open Core.Std

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

    let get_uint8  = Storage.get_uint8
    let get_uint16 = Storage.get_uint16
    let get_uint32 = Storage.get_uint32
    let get_uint64 = Storage.get_uint64
    let get_int8   = Storage.get_int8
    let get_int16  = Storage.get_int16
    let get_int32  = Storage.get_int32
    let get_int64  = Storage.get_int64
    let set_uint8  = Storage.set_uint8
    let set_uint16 = Storage.set_uint16
    let set_uint32 = Storage.set_uint32
    let set_uint64 = Storage.set_uint64
    let set_int8   = Storage.set_int8
    let set_int16  = Storage.set_int16
    let set_int32  = Storage.set_int32
    let set_int64  = Storage.set_int64
  end   (* module Segment *)

  module Message = struct
    type storage_t = Storage.t
    type -'cap segment_t = Storage.t

    type segment_descr_t = {
      segment : Storage.t;
      free_start : int;  (* Offset into segment where free space begins *)
    }

    type -'cap t = segment_descr_t Res.Array.t

    let create size =
      let segment = Storage.alloc (Util.round_up_mult_8 size) in
      Res.Array.create 1 {segment; free_start = 0}

    (** [add_segment m size] allocates a new segment of [size] bytes and appends
        it to the message, raising an exception if storage cannot be allocated. *)
    let add_segment m size =
      let new_segment = Storage.alloc (Util.round_up_mult_8 size) in
      Res.Array.add_one m {segment = new_segment; free_start = 0}

    let release m =
      let () = Res.Array.iter (fun descr -> Storage.release descr.segment) m in
      Res.Array.clear m

    let num_segments = Res.Array.length

    let total_size m =
      Res.Array.fold_left (fun acc x -> acc + (Segment.length x.segment)) 0 m

    let get_segment m i = (Res.Array.get m i).segment

    (** [get_size m] computes the aggregate size of the message, in bytes. *)
    let get_size m =
      (* Not too worried about using O(segments) implementation, because number
         of segments is O(log(message size)). *)
      Res.Array.fold_left (fun acc x -> acc + (Storage.length x.segment)) 0 m

    let readonly m = m

    let of_storage ms =
      let arr = Res.Array.empty () in
      let () = List.iter ms ~f:(fun x ->
          Res.Array.add_one arr {segment = x; free_start = Storage.length x}) in
      arr

    let to_storage m = Res.Array.fold_right (fun x acc -> x.segment :: acc) m []

    let with_message m ~f = Exn.protectx m ~f ~finally:release
  end   (* module Message *)

  module Slice = struct
    type -'cap segment_t = Storage.t
    type -'cap message_t = 'cap Message.t

    type 'cap t = {
      msg        : 'cap message_t;
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
        let last_seg_id = Res.Array.length m - 1 in
        let last_seg_descr = Res.Array.get m last_seg_id in
        let bytes_avail = Storage.length last_seg_descr.Message.segment -
          last_seg_descr.Message.free_start
        in
        if size <= bytes_avail then
          last_seg_id, last_seg_descr
        else
          (* Doubling message size on each allocation, under the assumption
             that message sizes will be exponentially distributed. *)
          let min_alloc_size = Message.get_size m in
          let new_seg = Storage.alloc (max min_alloc_size size) in
          let new_seg_descr = {Message.segment = new_seg; Message.free_start = 0} in
          let () = Res.Array.add_one m new_seg_descr in
          last_seg_id + 1, new_seg_descr
      in
      let slice = {
        msg = m;
        segment_id;
        start = segment_descr.Message.free_start;
        len = size;
      } in
      let () = Res.Array.set m segment_id
        { segment_descr with
          Message.free_start = slice.start + slice.len }
      in
      (* Allocations should be eight-byte aligned *)
      let () = assert ((slice.start land 7) = 0) in
      slice

    let alloc_in_segment m segment_id size =
      let size = Util.round_up_mult_8 size in
      let seg_descr = Res.Array.get m segment_id in
      let bytes_avail = Storage.length seg_descr.Message.segment -
          seg_descr.Message.free_start
      in
      if size <= bytes_avail then
        let slice = {
          msg = m;
          segment_id;
          start = seg_descr.Message.free_start;
          len = size;
        } in
        let () = Res.Array.set m segment_id
          { seg_descr with
            Message.free_start = slice.start + slice.len }
        in
        (* Allocations should be eight-byte aligned *)
        let () = assert ((slice.start land 7) = 0) in
        Some slice
      else
        None

    let get_segment slice = Message.get_segment slice.msg slice.segment_id

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

    (* TODO: this should delegate to a possibly-more-efficient [blit] provided by
       the underlying message (e.g. [String.blit]). *)
    let blit ~src ~src_ofs ~dest ~dest_ofs ~len =
      for i = 0 to len - 1 do
        let byte = get_uint8 src (src_ofs + i) in
        set_uint8 dest (dest_ofs + i) byte
      done

    (* TODO: again, could possibly delegate to a more efficient implementation *)
    let zero_out ~ofs ~len slice =
      for i = 0 to len - 1 do
        set_uint8 slice (ofs + i) 0
      done

  end   (* module Slice *)

  module StructStorage = struct
    (** Storage associated with a cap'n proto struct. *)
    type 'cap t = {
      data     : 'cap Slice.t;  (** Storage for struct fields stored by value *)
      pointers : 'cap Slice.t;  (** Storage for struct fields stored by reference *)
    }

    let readonly (struct_storage : 'cap t) : ro t = {
      data     = Slice.readonly struct_storage.data;
      pointers = Slice.readonly struct_storage.pointers;
    }
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
    type 'cap t =
      | None
      | List of 'cap ListStorage.t
      | Struct of 'cap StructStorage.t
      | Capability of Uint32.t
  end
end


exception Invalid_message of string
let invalid_msg s = raise (Invalid_message s)


