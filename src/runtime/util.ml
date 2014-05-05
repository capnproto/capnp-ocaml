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

(* Decode [num] as a signed integer of width [n] bits, using two's complement
   representation of negative numbers. *)
let decode_signed n num =
  let power_of_two = 1 lsl (n - 1) in
  let is_signed = (num land power_of_two) <> 0 in
  if is_signed then
    (num land (power_of_two - 1)) - power_of_two
  else
    num


(* Encode signed integer [num] into [n] bits, using two's complement
   representation of negative numbers. *)
let encode_signed n num =
  if num >= 0 then
    num
  else
    (1 lsl (n - 1)) + num


let ceil_int num denom = (num + denom - 1) / denom


let uint64_equal a b : bool = Uint64.compare a b = 0


let round_up_mult_8 (x : int) : int =
  (x + 7) land (lnot 7)


(* I think the semantics of Core_string.slice are kind of dangerous.
   stop=0 is special-cased in a way which could lead to bugs:
   [Core_string.slice s 0 0] will return a copy of s, when the user
   may have intended to generate an empty string.

   This variant parallels the behavior of Python's slicing operator. *)
let str_slice ?(start : int option) ?(stop : int option) (s : string)
  : string =
  let norm s i = Core.Ordered_collection_common.normalize
      ~length_fun:String.length s i
  in
  let real_start =
    match start with
    | Some x -> norm s x
    | None   -> 0
  in
  let real_stop =
    match stop with
    | Some x -> norm s x
    | None   -> String.length s
  in
  String.sub s real_start (real_stop - real_start)

