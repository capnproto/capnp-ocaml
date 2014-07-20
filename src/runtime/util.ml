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

exception Out_of_int_range of string
let out_of_int_range s = raise (Out_of_int_range s)


(* Decode [num] as a signed integer of width [n] bits, using two's complement
   representation of negative numbers. *)
let decode_signed n num =
  let () = assert (n < Int.num_bits) in
  let power_of_two = 1 lsl (n - 1) in
  let is_signed = (num land power_of_two) <> 0 in
  if is_signed then
    (num land (power_of_two - 1)) - power_of_two
  else
    num


(* Encode signed integer [num] into [n] bits, using two's complement
   representation of negative numbers. *)
let encode_signed n num =
  let () = assert (n < Int.num_bits) in
  if num >= 0 then
    num
  else
    (1 lsl n) + num


(* [ceil_ratio n m] computes ceiling(n/m) *)
let ceil_ratio num denom = (num + denom - 1) / denom


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


let int_of_int32_exn : int32 -> int =
  if Sys.word_size = 32 then
    let max_val = Int32.of_int_exn Int.max_value in
    let min_val = Int32.of_int_exn Int.min_value in
    (fun i32 ->
       if Int32.compare i32 min_val < 0 ||
          Int32.compare i32 max_val > 0 then
         out_of_int_range "Int32"
       else
         Caml.Int32.to_int i32)
  else
    Caml.Int32.to_int

let int_of_int64_exn : int64 -> int =
  let max_val = Int64.of_int_exn Int.max_value in
  let min_val = Int64.of_int_exn Int.min_value in
  (fun i64 ->
     if Int64.compare i64 min_val < 0 ||
        Int64.compare i64 max_val > 0 then
       out_of_int_range "Int64"
     else
       Caml.Int64.to_int i64)

let int_of_uint32_exn : Uint32.t -> int =
  if Sys.word_size = 32 then
    let max_val = Uint32.of_int Int.max_value in
    (fun u32 ->
       if Uint32.compare u32 max_val > 0 then
         out_of_int_range "UInt32"
       else
         Uint32.to_int u32)
  else
    Uint32.to_int

let int_of_uint64_exn : Uint64.t -> int =
  let max_val = Uint64.of_int Int.max_value in
  (fun u64 ->
     if Uint64.compare u64 max_val > 0 then
       out_of_int_range "UInt64"
     else
       Uint64.to_int u64)

let int32_of_int_exn : int -> int32 =
  if Sys.word_size = 64 then
    let max_val = Int32.to_int_exn (Int32.max_value) in
    let min_val = Int32.to_int_exn (Int32.min_value) in
    (fun i ->
       if i < min_val || i > max_val then
         invalid_arg "Int32.of_int"
       else
         Caml.Int32.of_int i)
  else
    Caml.Int32.of_int

let uint32_of_int_exn : int -> Uint32.t =
  if Sys.word_size = 64 then
    let max_val = Uint32.to_int (Uint32.max_int) in
    (fun i ->
       if i < 0 || i > max_val then
         invalid_arg "Uint32.of_int"
       else
         Uint32.of_int i)
  else
    Uint32.of_int

let uint64_of_int_exn i =
  if i < 0 then
    invalid_arg "Uint64.of_int"
  else
    Uint64.of_int i


let hex_table = [|
  '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7';
  '8'; '9'; 'a'; 'b'; 'c'; 'd'; 'e'; 'f';
|]

(* [String.escaped] produces valid data, but it's not the easiest to try to read
   the octal format.  Generate hex instead. *)
let make_hex_literal s =
  let result = Bytes.create ((String.length s) * 4) in
  for i = 0 to String.length s - 1 do
    let byte = Char.to_int s.[i] in
    let upper_nibble = (byte lsr 4) land 0xf in
    let lower_nibble = byte land 0xf in
    Bytes.set result ((4 * i) + 0) '\\';
    Bytes.set result ((4 * i) + 1) 'x';
    Bytes.set result ((4 * i) + 2) hex_table.(upper_nibble);
    Bytes.set result ((4 * i) + 3) hex_table.(lower_nibble);
  done;
  Bytes.to_string result


