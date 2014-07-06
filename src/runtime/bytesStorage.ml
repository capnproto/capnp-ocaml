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
open EndianBytes

type t = Bytes.t

let alloc size = Bytes.make size '\x00'
let release x = ()

let length = Bytes.length

let get_uint8      = LittleEndian.get_uint8
let get_uint16     = LittleEndian.get_uint16
let get_uint32 s i = Uint32.of_int32 (LittleEndian.get_int32 s i)
let get_uint64 s i = Uint64.of_int64 (LittleEndian.get_int64 s i)

let get_int8  = LittleEndian.get_int8
let get_int16 = LittleEndian.get_int16
let get_int32 = LittleEndian.get_int32
let get_int64 = LittleEndian.get_int64

let set_uint32 s i v = LittleEndian.set_int32 s i (Uint32.to_int32 v)
let set_uint64 s i v = LittleEndian.set_int64 s i (Uint64.to_int64 v)

let set_int32 = LittleEndian.set_int32
let set_int64 = LittleEndian.set_int64

let set_uint8 s i v =
  if v < 0 || v > 0xff then
    invalid_arg "BytesStorage.set_uint8"
  else
    (* Not a typo!  [LittleEndian.set_int8] "writes the least significant
       8 bits of [v]", which is the behavior we want from [set_uint8]. *)
    LittleEndian.set_int8 s i v

let set_uint16 s i v =
  if v < 0 || v > 0xffff then
    invalid_arg "BytesStorage.set_uint16"
  else
    LittleEndian.set_int16 s i v

let set_int8 s i v =
  if v < -128 || v > 127 then
    invalid_arg "BytesStorage.set_int8"
  else
    LittleEndian.set_int8 s i v

let set_int16 s i v =
  if v < -32768 || v > 32767 then
    invalid_arg "BytesStorage.set_int16"
  else
    let u16 =
      if v < 0 then
        (1 lsl 16) - (-v)
      else
        v
    in
    LittleEndian.set_int16 s i u16


