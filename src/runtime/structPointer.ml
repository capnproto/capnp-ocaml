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


type t = {
  (** Signed offset in words from end of the pointer to start of struct
      data region. *)
  offset : int;

  (** Size of struct data region, in words. *)
  data_words : int;

  (** Size of struct pointers region, in words. *)
  pointer_words : int;
}


let tag_val_struct = 0L

let offset_shift    = 2
let offset_mask     = Int64.shift_left 0x3fffffffL offset_shift
let offset_mask_int = 0x3fffffff lsl offset_shift

let data_size_shift    = 32
let data_size_mask     = Int64.shift_left 0xffffL data_size_shift
let data_size_mask_int = 0xffff lsl data_size_shift

let pointers_size_shift = 48

let decode (pointer64 : Int64.t) : t =
  let pointers_size =
    let shifted64 = Int64.shift_right_logical pointer64 pointers_size_shift in
    (* pointers size is left-aligned, no need to mask it *)
    Int64.to_int shifted64
  in
  (* Int64 arithmetic causes unfortunate GC pressure.  If we're on a 64-bit
     platform, use standard 63-bit ints whenever possible. *)
  if Sys.word_size = 64 then
    let pointer = Int64.to_int pointer64 in
    let offset =
      let v = (pointer land offset_mask_int) lsr offset_shift in
      Util.decode_signed 30 v
    in
    let data_size =
      (pointer land data_size_mask_int) lsr data_size_shift
    in {
      offset;
      data_words    = data_size;
      pointer_words = pointers_size;
    }
  else
    let offset =
      let masked     = Int64.logand pointer64 offset_mask in
      let offset64   = Int64.shift_right_logical masked offset_shift in
      let offset_int = Int64.to_int offset64 in
      Util.decode_signed 30 offset_int
    in
    let data_size =
      let masked = Int64.logand pointer64 data_size_mask in
      let size64 = Int64.shift_right_logical masked data_size_shift in
      Int64.to_int size64
    in {
      offset;
      data_words    = data_size;
      pointer_words = pointers_size;
    }


let encode (storage_descr : t) : Int64.t =
  (* Int64 arithmetic causes unfortunate GC pressure.  If we're on a 64-bit
     platform, use standard 63-bit ints whenever possible. *)
  if Sys.word_size = 64 && storage_descr.pointer_words <= 0x7fff then
    let offset = Util.encode_signed 30 storage_descr.offset in
    let data_size = storage_descr.data_words in
    let ptr_size = storage_descr.pointer_words in
    let tag_val_struct_int = 0 in
    Int64.of_int
      (tag_val_struct_int lor
         (offset lsl offset_shift) lor
         (data_size lsl data_size_shift) lor
         (ptr_size lsl pointers_size_shift))
  else
    let offset64 = Int64.of_int (Util.encode_signed 30 storage_descr.offset) in
    let data_size64 = Int64.of_int storage_descr.data_words in
    let ptr_size64 = Int64.of_int storage_descr.pointer_words in
    tag_val_struct |>
    Int64.logor (Int64.shift_left offset64 offset_shift) |>
    Int64.logor (Int64.shift_left data_size64 data_size_shift) |>
    Int64.logor (Int64.shift_left ptr_size64 pointers_size_shift)


