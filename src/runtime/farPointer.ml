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


let tag_val_far = Int64.of_int 0x2

let landing_pad_type_shift = 2
let landing_pad_type_mask  = Int64.shift_left Int64.one landing_pad_type_shift

let offset_shift = 3
let offset_mask  = Int64.shift_left (Int64.of_int 0x1fffffff) offset_shift

let segment_shift = 32
let segment_mask  =
  Int64.shift_left (Int64.(-) (Int64.shift_left Int64.one 32) Int64.one) segment_shift

let decode (pointer64 : Int64.t) : t =
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
    Int64.to_int_exn offset64
  in
  let segment_id =
    let max64  = Int64.of_int max_int in
    let masked = Int64.bit_and pointer64 segment_mask in
    let id64   = Int64.shift_right_logical masked segment_shift in
    if Int64.compare id64 max64 > 0 then
      Message.invalid_msg "far pointer contains segment ID larger than OCaml max_int"
    else
      Int64.to_int_exn id64
  in {
    landing_pad;
    offset;
    segment_id;
  }


let encode (storage_descr : t) : Int64.t =
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


