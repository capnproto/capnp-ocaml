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

type t =
  | Capability of Uint32.t

let tag_val_other = 0x3L

let b_shift = 2
let b_mask  = Int64.shift_left 0x3fffffffL b_shift

let index_shift = 32
let index_mask  = Int64.shift_left 0xffffffffL index_shift

let decode (pointer64 : Int64.t) : t =
  if Int64.compare (Int64.logand pointer64 b_mask) Int64.zero = 0 then
    let shifted_index = Int64.logand pointer64 index_mask in
    let index64 = Int64.shift_right_logical shifted_index index_shift in
    let index32 = Int64.to_int32 index64 in
    Capability (Uint32.of_int32 index32)
  else
    Message.invalid_msg "'other' pointer is of non-capability type"

let encode (descr : t) : Int64.t =
  match descr with
  | Capability index ->
      let index32 = Uint32.to_int32 index in
      let index64 = Int64.of_int32 index32 in
      let shifted_index = Int64.shift_left index64 index_shift in
      Int64.logor shifted_index tag_val_other


