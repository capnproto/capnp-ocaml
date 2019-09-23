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

module Uint32 = Stdint.Uint32
module Uint64 = Stdint.Uint64

module type S = sig
  (** [t] is the type of the underlying storage used for message segments. *)
  type t

  (** [alloc size] allocates storage for [size] bytes, raising an exception
      if storage cannot be allocated. *)
  val alloc : int -> t

  (** [release s] immediately releases storage [s], potentially making the
      storage available for future allocations.  After releasing a
      storage segment, the behavior of the accessor functions is undefined. *)
  val release : t -> unit

  (** [length s] determines the length of this storage. *)
  val length : t -> int

  (** [get_uintXX s ofs] reads an unsigned integer of the specified width,
      starting at byte offset [ofs] within the message segment. *)

  val get_uint8  : t -> int -> int
  val get_uint16 : t -> int -> int
  val get_uint32 : t -> int -> Uint32.t
  val get_uint64 : t -> int -> Uint64.t

  (** [get_intXX s ofs] reads a signed integer of the specified width,
      starting at byte offset [ofs] within the message segment. *)

  val get_int8   : t -> int -> int
  val get_int16  : t -> int -> int
  val get_int32  : t -> int -> Int32.t
  val get_int64  : t -> int -> Int64.t

  (** [set_uintXX s ofs val] writes the value of the width-restricted
      unsigned integer [val] into the message segment, starting at
      byte offset [ofs]. *)

  val set_uint8  : t -> int -> int -> unit
  val set_uint16 : t -> int -> int -> unit
  val set_uint32 : t -> int -> Uint32.t -> unit
  val set_uint64 : t -> int -> Uint64.t -> unit

  (** [set_intXX s ofs val] writes the value of the width-restricted
      signed integer [val] into the message segment, starting at
      byte offset [ofs]. *)

  val set_int8   : t -> int -> int -> unit
  val set_int16  : t -> int -> int -> unit
  val set_int32  : t -> int -> Int32.t -> unit
  val set_int64  : t -> int -> Int64.t -> unit

  (** [blit ~src ~src_pos ~dst ~dst_pos ~len] transfers [len] bytes
      from position [dst_pos] in [dst] to position [src_pos] in [pos].
      The blit operation shall work correctly even for the case of
      overlapping buffers. *)
  val blit : src:t -> src_pos:int -> dst:t -> dst_pos:int -> len:int -> unit

  (** As [blit], but the destination is a [bytes] buffer. *)
  val blit_to_bytes : src:t -> src_pos:int ->
    dst:Bytes.t -> dst_pos:int -> len:int -> unit

  (** As [blit], but the source is a [string] buffer. *)
  val blit_from_string : src:string -> src_pos:int ->
    dst:t -> dst_pos:int -> len:int -> unit

  (** [zero_out segment ~pos ~len] sets [len] bytes of the segment
      to zero, beginning at byte offset [pos]. *)
  val zero_out : t -> pos:int -> len:int -> unit
end

