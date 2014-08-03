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


module Int64 = Core.Core_int64
module Caml  = Core.Caml

type landing_pad_t =
  | NormalPointer
  | TaggedFarPointer

type t = {
  (** Type of landing pad which this far pointer points to. *)
  landing_pad : landing_pad_t;

  (** Unsigned offset in words from start of the segment to start of
      the landing pad. *)
  offset : int;

  (** Segment ID where the landing pad is located. *)
  segment_id : int
}


let tag_val_far = 0x2L

let landing_pad_type_shift = 2
let landing_pad_type_mask  = Int64.shift_left 1L landing_pad_type_shift
let landing_pad_type_mask_int = 1 lsl landing_pad_type_shift

let offset_shift = 3
let offset_mask  = Int64.shift_left 0x1fffffffL offset_shift
let offset_mask_int = 0x1fffffff lsl offset_shift

let segment_shift = 32
let segment_mask  = Int64.shift_left 0xffffffffL segment_shift

let decode (pointer64 : Int64.t) : t =
  (* Int64 arithmetic causes unfortunate GC pressure.  If we're on a 64-bit
     platform, use standard 63-bit ints whenever possible. *)
  if Sys.word_size = 64 then
    let segment_id =
      let id64 = Int64.shift_right_logical pointer64 segment_shift in
      Caml.Int64.to_int id64
    in
    let pointer = Caml.Int64.to_int pointer64 in
    let landing_pad =
      if (pointer land landing_pad_type_mask_int) = 0 then
        NormalPointer
      else
        TaggedFarPointer
    in
    let offset =
      (pointer land offset_mask_int) lsr offset_shift
    in {
      landing_pad;
      offset;
      segment_id;
    }
  else
    let segment_id =
      let max64 = Int64.of_int max_int in
      (* Segment ID is left-aligned, no need to mask it *)
      let id64 = Int64.shift_right_logical pointer64 segment_shift in
      if Int64.compare id64 max64 > 0 then
        Message.invalid_msg "far pointer contains segment ID larger than OCaml max_int"
      else
        Caml.Int64.to_int id64
    in
    let landing_pad =
      let masked = Int64.bit_and pointer64 landing_pad_type_mask in
      if Int64.compare masked Int64.zero = 0 then
        NormalPointer
      else
        TaggedFarPointer
    in
    let offset =
      let masked = Int64.bit_and pointer64 offset_mask in
      let offset64 = Int64.shift_right_logical masked offset_shift in
      Caml.Int64.to_int offset64
    in {
      landing_pad;
      offset;
      segment_id;
    }


let encode (storage_descr : t) : Int64.t =
  (* Int64 arithmetic causes unfortunate GC pressure.  If we're on a 64-bit
     platform, use standard 63-bit ints whenever possible. *)
  if Sys.word_size = 64 && storage_descr.segment_id <= 0x7fffffff then
    let tp =
      match storage_descr.landing_pad with
      | NormalPointer    -> 0
      | TaggedFarPointer -> 1
    in
    let tag_val_far_int = 2 in
    Int64.of_int
      (tag_val_far_int lor
         (tp lsl landing_pad_type_shift) lor
         (storage_descr.offset lsl offset_shift) lor
         (storage_descr.segment_id lsl segment_shift))
  else
    let type64 =
      match storage_descr.landing_pad with
      | NormalPointer ->
          Int64.zero
      | TaggedFarPointer ->
          Int64.one
    in
    let offset64 = Int64.of_int storage_descr.offset in
    let segment64 = Int64.of_int storage_descr.segment_id in
    tag_val_far |>
    Int64.bit_or (Int64.shift_left type64 landing_pad_type_shift) |>
    Int64.bit_or (Int64.shift_left offset64 offset_shift) |>
    Int64.bit_or (Int64.shift_left segment64 segment_shift)


