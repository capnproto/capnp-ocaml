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

open OUnit2

let encode_signed_suite =
  let open Capnp.Runtime.Util in
  let t0 _ctx = assert_equal (encode_signed 17 0) 0 in
  let t1 _ctx = assert_equal (encode_signed 17 0xffff) 0xffff in
  let t2 _ctx = assert_equal (encode_signed 17 (-0x10000)) 0x10000 in
  let t3 _ctx = assert_equal (encode_signed 17 0x1234) 0x1234 in
  let t4 _ctx = assert_equal (encode_signed 17 (-0x1234)) 0x1edcc in
  "encode_signed" >::: [
    "zero" >:: t0;
    "max" >:: t1;
    "min" >:: t2;
    "random positive" >:: t3;
    "random negative" >:: t4;
  ]

let decode_signed_suite =
  let open Capnp.Runtime.Util in
  let t0 _ctx = assert_equal (decode_signed 17 0) 0 in
  let t1 _ctx = assert_equal (decode_signed 17 0xffff) 0xffff in
  let t2 _ctx = assert_equal (decode_signed 17 0x10000) (-0x10000) in
  let t3 _ctx = assert_equal (decode_signed 17 0x1234) 0x1234 in
  let t4 _ctx = assert_equal (decode_signed 17 0x1edcc) (-0x1234) in
  "decode_signed" >::: [
    "zero" >:: t0;
    "max" >:: t1;
    "min" >:: t2;
    "random positive" >:: t3;
    "random negative" >:: t4;
  ]


