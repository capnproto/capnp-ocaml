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


module SM = Capnp.Message.Make(Capnp.BytesStorage)
open OUnit2


let assert_raises_invalid_arg f =
  try
    let _ = f () in
    assert_failure "did not raise Invalid_argument"
  with Invalid_argument _ ->
    ()


let uint8_suite =
  let short_msg = SM.Message.of_storage
      [ Bytes.of_string "\x11\x00\xff\x44\x55\x66\x77\x88" ]
  in
  let short_slice = {
    SM.Slice.msg        = short_msg;
    SM.Slice.segment    = SM.Message.get_segment short_msg 0;
    SM.Slice.segment_id = 0;
    SM.Slice.start      = 0;
    SM.Slice.len        = SM.Message.total_size short_msg;
  } in
  let t0 ctx = assert_equal (SM.Slice.get_uint8 short_slice 0) 0x11 in
  let t1 ctx = assert_equal (SM.Slice.get_uint8 short_slice 7) 0x88 in
  let t2 ctx = assert_equal (SM.Slice.get_uint8 short_slice 1) 0x00 in
  let t3 ctx = assert_equal (SM.Slice.get_uint8 short_slice 2) 0xff in
  let t4 ctx = assert_raises
      (Invalid_argument "Slice.get_uint8")
      (fun () -> SM.Slice.get_uint8 short_slice (-1))
  in
  let t5 ctx = assert_raises
      (Invalid_argument "Slice.get_uint8")
      (fun () -> SM.Slice.get_uint8 short_slice 8)
  in
  let t6 ctx = assert_raises
      (Invalid_argument "Slice.set_uint8")
      (fun () -> SM.Slice.set_uint8 short_slice (-1) 0xaa)
  in
  let t7 ctx = assert_raises
      (Invalid_argument "Slice.set_uint8")
      (fun () -> SM.Slice.set_uint8 short_slice 8 0xaa)
  in
  let t8 ctx =
    let () = SM.Slice.set_uint8 short_slice 0 0x00 in
    assert_equal (SM.Slice.get_uint8 short_slice 0) 0x00
  in
  let t9 ctx =
    let () = SM.Slice.set_uint8 short_slice 7 0xff in
    assert_equal (SM.Slice.get_uint8 short_slice 7) 0xff
  in
  let t10 ctx = assert_raises_invalid_arg
      (fun () -> SM.Slice.set_uint8 short_slice 0 (-1))
  in
  let t11 ctx = assert_raises_invalid_arg
      (fun () -> SM.Slice.set_uint8 short_slice 0 0x100)
  in
  "slice_uint8" >::: [
    "get first" >:: t0;
    "get last" >:: t1;
    "get value 0x00" >:: t2;
    "get value 0xff" >:: t3;
    "get below range" >:: t4;
    "get above range" >:: t5;
    "set below range" >:: t6;
    "set above range" >:: t7;
    "set first" >:: t8;
    "set last" >:: t9;
    "set value -1" >:: t10;
    "set value 0x100" >:: t11;
  ]


let uint16_suite =
  let short_msg = SM.Message.of_storage
      [ Bytes.of_string "\x11\x00\x00\xff\xff\x66\x77\x88" ]
  in
  let short_slice = {
    SM.Slice.msg        = short_msg;
    SM.Slice.segment    = SM.Message.get_segment short_msg 0;
    SM.Slice.segment_id = 0;
    SM.Slice.start      = 0;
    SM.Slice.len        = SM.Message.total_size short_msg;
  } in
  let t0 ctx = assert_equal (SM.Slice.get_uint16 short_slice 0) 0x0011 in
  let t1 ctx = assert_equal (SM.Slice.get_uint16 short_slice 6) 0x8877 in
  let t2 ctx = assert_equal (SM.Slice.get_uint16 short_slice 1) 0x0000 in
  let t3 ctx = assert_equal (SM.Slice.get_uint16 short_slice 3) 0xffff in
  let t4 ctx = assert_raises
      (Invalid_argument "Slice.get_uint16")
      (fun () -> SM.Slice.get_uint16 short_slice (-1))
  in
  let t5 ctx = assert_raises
      (Invalid_argument "Slice.get_uint16")
      (fun () -> SM.Slice.get_uint16 short_slice 7)
  in
  let t6 ctx = assert_raises
      (Invalid_argument "Slice.set_uint16")
      (fun () -> SM.Slice.set_uint16 short_slice (-1) 0xaaaa)
  in
  let t7 ctx = assert_raises
      (Invalid_argument "Slice.set_uint16")
      (fun () -> SM.Slice.set_uint16 short_slice 7 0xaaaa)
  in
  let t8 ctx =
    let () = SM.Slice.set_uint16 short_slice 0 0x0000 in
    assert_equal (SM.Slice.get_uint16 short_slice 0) 0x0000
  in
  let t9 ctx =
    let () = SM.Slice.set_uint16 short_slice 6 0xffff in
    assert_equal (SM.Slice.get_uint16 short_slice 6) 0xffff
  in
  let t10 ctx = assert_raises_invalid_arg
      (fun () -> SM.Slice.set_uint16 short_slice 0 (-1))
  in
  let t11 ctx = assert_raises_invalid_arg
      (fun () -> SM.Slice.set_uint16 short_slice 0 0x10000)
  in
  "slice_uint16" >::: [
    "get first" >:: t0;
    "get last" >:: t1;
    "get value 0x0000" >:: t2;
    "get value 0xffff" >:: t3;
    "get below range" >:: t4;
    "get above range" >:: t5;
    "set below range" >:: t6;
    "set above range" >:: t7;
    "set first" >:: t8;
    "set last" >:: t9;
    "set value -1" >:: t10;
    "set value 0x10000" >:: t11;
  ]


let uint32_suite =
  let short_msg = SM.Message.of_storage
      [ Bytes.of_string "\x11\x22\x33\x44\x55\x66\x77\x88" ]
  in
  let short_slice = {
    SM.Slice.msg        = short_msg;
    SM.Slice.segment    = SM.Message.get_segment short_msg 0;
    SM.Slice.segment_id = 0;
    SM.Slice.start      = 0;
    SM.Slice.len        = SM.Message.total_size short_msg;
  } in
  let t0 ctx = assert_equal (SM.Slice.get_uint32 short_slice 0)
      (Uint32.of_string "0x44332211")
  in
  let t1 ctx = assert_equal (SM.Slice.get_uint32 short_slice 4)
      (Uint32.of_string "0x88776655")
  in
  let t2 ctx = assert_raises
      (Invalid_argument "Slice.get_uint32")
      (fun () -> SM.Slice.get_uint32 short_slice (-1))
  in
  let t3 ctx = assert_raises
      (Invalid_argument "Slice.get_uint32")
      (fun () -> SM.Slice.get_uint32 short_slice 5)
  in
  let t4 ctx = assert_raises
      (Invalid_argument "Slice.set_uint32")
      (fun () -> SM.Slice.set_uint32 short_slice (-1) Uint32.zero)
  in
  let t5 ctx = assert_raises
      (Invalid_argument "Slice.set_uint32")
      (fun () -> SM.Slice.set_uint32 short_slice 5 Uint32.zero)
  in
  let t6 ctx =
    let () = SM.Slice.set_uint32 short_slice 4 Uint32.min_int in
    assert_equal (SM.Slice.get_uint32 short_slice 4) Uint32.min_int
  in
  let t7 ctx =
    let () = SM.Slice.set_uint32 short_slice 0 Uint32.max_int in
    assert_equal (SM.Slice.get_uint32 short_slice 0) Uint32.max_int
  in
  "slice_uint32" >::: [
    "get first" >:: t0;
    "get last" >:: t1;
    "get below range" >:: t2;
    "get above range" >:: t3;
    "set below range" >:: t4;
    "set above range" >:: t5;
    "set last" >:: t6;
    "set first" >:: t7;
  ]


let uint64_suite =
  let short_msg = SM.Message.of_storage [
    Bytes.of_string
      "\x11\x22\x33\x44\x55\x66\x77\x88\
       \x99\xaa\xbb\xcc\xdd\xee\xff\xa5"
  ] in
  let short_slice = {
    SM.Slice.msg        = short_msg;
    SM.Slice.segment    = SM.Message.get_segment short_msg 0;
    SM.Slice.segment_id = 0;
    SM.Slice.start      = 0;
    SM.Slice.len        = SM.Message.total_size short_msg;
  } in
  let t0 ctx = assert_equal (SM.Slice.get_uint64 short_slice 0)
      (Uint64.of_string "0x8877665544332211")
  in
  let t1 ctx = assert_equal (SM.Slice.get_uint64 short_slice 8)
      (Uint64.of_string "0xa5ffeeddccbbaa99")
  in
  let t2 ctx = assert_raises
      (Invalid_argument "Slice.get_uint64")
      (fun () -> SM.Slice.get_uint64 short_slice (-1))
  in
  let t3 ctx = assert_raises
      (Invalid_argument "Slice.get_uint64")
      (fun () -> SM.Slice.get_uint64 short_slice 9)
  in
  let t4 ctx = assert_raises
      (Invalid_argument "Slice.set_uint64")
      (fun () -> SM.Slice.set_uint64 short_slice (-1) Uint64.zero)
  in
  let t5 ctx = assert_raises
      (Invalid_argument "Slice.set_uint64")
      (fun () -> SM.Slice.set_uint64 short_slice 9 Uint64.zero)
  in
  let t6 ctx =
    let () = SM.Slice.set_uint64 short_slice 8 Uint64.min_int in
    assert_equal (SM.Slice.get_uint64 short_slice 8) Uint64.min_int
  in
  let t7 ctx =
    let () = SM.Slice.set_uint64 short_slice 0 Uint64.max_int in
    assert_equal (SM.Slice.get_uint64 short_slice 0) Uint64.max_int
  in
  "slice_uint64" >::: [
    "get first" >:: t0;
    "get last" >:: t1;
    "get below range" >:: t2;
    "get above range" >:: t3;
    "set below range" >:: t4;
    "set above range" >:: t5;
    "set last" >:: t6;
    "set first" >:: t7;
  ]


let int8_suite =
  let short_msg = SM.Message.of_storage
      [ Bytes.of_string "\x11\x00\xff\x44\x55\x66\x77\x88"]
  in
  let short_slice = {
    SM.Slice.msg        = short_msg;
    SM.Slice.segment    = SM.Message.get_segment short_msg 0;
    SM.Slice.segment_id = 0;
    SM.Slice.start      = 0;
    SM.Slice.len        = SM.Message.total_size short_msg;
  } in
  let t0 ctx = assert_equal (SM.Slice.get_int8 short_slice 0) 0x11 in
  let t1 ctx = assert_equal (SM.Slice.get_int8 short_slice 7) (-0x78) in
  let t2 ctx = assert_equal (SM.Slice.get_int8 short_slice 1) 0x00 in
  let t3 ctx = assert_equal (SM.Slice.get_int8 short_slice 2) (-0x01) in
  let t4 ctx = assert_raises
      (Invalid_argument "Slice.get_int8")
      (fun () -> SM.Slice.get_int8 short_slice (-1))
  in
  let t5 ctx = assert_raises
      (Invalid_argument "Slice.get_int8")
      (fun () -> SM.Slice.get_int8 short_slice 8)
  in
  let t6 ctx = assert_raises
      (Invalid_argument "Slice.set_int8")
      (fun () -> SM.Slice.set_int8 short_slice (-1) 0x0a)
  in
  let t7 ctx = assert_raises
      (Invalid_argument "Slice.set_int8")
      (fun () -> SM.Slice.set_int8 short_slice 8 0x0a)
  in
  let t8 ctx =
    let () = SM.Slice.set_int8 short_slice 0 (-128) in
    assert_equal (SM.Slice.get_int8 short_slice 0) (-128)
  in
  let t9 ctx =
    let () = SM.Slice.set_int8 short_slice 7 127 in
    assert_equal (SM.Slice.get_int8 short_slice 7) 127
  in
  let t10 ctx = assert_raises_invalid_arg
      (fun () -> SM.Slice.set_int8 short_slice 0 (-129))
  in
  let t11 ctx = assert_raises_invalid_arg
      (fun () -> SM.Slice.set_int8 short_slice 0 128)
  in
  "slice_int8" >::: [
    "get first" >:: t0;
    "get last" >:: t1;
    "get value 0x00" >:: t2;
    "get value -0x01" >:: t3;
    "get below range" >:: t4;
    "get above range" >:: t5;
    "set below range" >:: t6;
    "set above range" >:: t7;
    "set first" >:: t8;
    "set last" >:: t9;
    "set value -129" >:: t10;
    "set value 128" >:: t11;
  ]

let int16_suite =
  let short_msg = SM.Message.of_storage
      [ Bytes.of_string "\x11\x00\x00\xff\xff\x66\x77\x88" ]
  in
  let short_slice = {
    SM.Slice.msg        = short_msg;
    SM.Slice.segment    = SM.Message.get_segment short_msg 0;
    SM.Slice.segment_id = 0;
    SM.Slice.start      = 0;
    SM.Slice.len        = SM.Message.total_size short_msg;
  } in
  let t0 ctx = assert_equal (SM.Slice.get_int16 short_slice 0) 0x11 in
  let t1 ctx = assert_equal (SM.Slice.get_int16 short_slice 6) (-0x7789) in
  let t2 ctx = assert_equal (SM.Slice.get_int16 short_slice 1) 0x0000 in
  let t3 ctx = assert_equal (SM.Slice.get_int16 short_slice 3) (-0x0001) in
  let t4 ctx = assert_raises
      (Invalid_argument "Slice.get_int16")
      (fun () -> SM.Slice.get_int16 short_slice (-1))
  in
  let t5 ctx = assert_raises
      (Invalid_argument "Slice.get_int16")
      (fun () -> SM.Slice.get_int16 short_slice 7)
  in
  let t6 ctx = assert_raises
      (Invalid_argument "Slice.set_int16")
      (fun () -> SM.Slice.set_int16 short_slice (-1) 0x0a)
  in
  let t7 ctx = assert_raises
      (Invalid_argument "Slice.set_int16")
      (fun () -> SM.Slice.set_int16 short_slice 7 0x0a)
  in
  let t8 ctx =
    let () = SM.Slice.set_int16 short_slice 0 (-32768) in
    assert_equal (SM.Slice.get_int16 short_slice 0) (-32768)
  in
  let t9 ctx =
    let () = SM.Slice.set_int16 short_slice 6 32767 in
    assert_equal (SM.Slice.get_int16 short_slice 6) 32767
  in
  let t10 ctx = assert_raises_invalid_arg
      (fun () -> SM.Slice.set_int16 short_slice 0 (-32769))
  in
  let t11 ctx = assert_raises_invalid_arg
      (fun () -> SM.Slice.set_int16 short_slice 0 32768)
  in
  "slice_int16" >::: [
    "get first" >:: t0;
    "get last" >:: t1;
    "get value 0x0000" >:: t2;
    "get value -0x0001" >:: t3;
    "get below range" >:: t4;
    "get above range" >:: t5;
    "set below range" >:: t6;
    "set above range" >:: t7;
    "set first" >:: t8;
    "set last" >:: t9;
    "set value -32769" >:: t10;
    "set value 32768" >:: t11;
  ]


let int32_suite =
  let short_msg = SM.Message.of_storage
      [ Bytes.of_string "\x11\x22\x33\x44\x55\x66\x77\x88" ]
  in
  let short_slice = {
    SM.Slice.msg        = short_msg;
    SM.Slice.segment    = SM.Message.get_segment short_msg 0;
    SM.Slice.segment_id = 0;
    SM.Slice.start      = 0;
    SM.Slice.len        = SM.Message.total_size short_msg;
  } in
  let t0 ctx = assert_equal (SM.Slice.get_int32 short_slice 0) 0x44332211l in
  let t1 ctx = assert_equal (SM.Slice.get_int32 short_slice 4) (-0x778899abl) in
  let t2 ctx = assert_raises
      (Invalid_argument "Slice.get_int32")
      (fun () -> SM.Slice.get_int32 short_slice (-1))
  in
  let t3 ctx = assert_raises
      (Invalid_argument "Slice.get_int32")
      (fun () -> SM.Slice.get_int32 short_slice 5)
  in
  let t4 ctx = assert_raises
      (Invalid_argument "Slice.set_int32")
      (fun () -> SM.Slice.set_int32 short_slice (-1) Int32.zero)
  in
  let t5 ctx = assert_raises
      (Invalid_argument "Slice.set_int32")
      (fun () -> SM.Slice.set_int32 short_slice 5 Int32.zero)
  in
  let t6 ctx =
    let () = SM.Slice.set_int32 short_slice 4 Int32.max_int in
    assert_equal (SM.Slice.get_int32 short_slice 4) Int32.max_int
  in
  let t7 ctx =
    let () = SM.Slice.set_int32 short_slice 0 Int32.min_int in
    assert_equal (SM.Slice.get_int32 short_slice 0) Int32.min_int
  in
  "slice_int32" >::: [
    "get first" >:: t0;
    "get last" >:: t1;
    "get below range" >:: t2;
    "get above range" >:: t3;
    "set below range" >:: t4;
    "set above range" >:: t5;
    "set last" >:: t6;
    "set first" >:: t7;
  ]


let int64_suite =
  let short_msg = SM.Message.of_storage [
    Bytes.of_string
      "\x11\x22\x33\x44\x55\x66\x77\x5a\
       \x99\xaa\xbb\xcc\xdd\xee\xff\xa5"
  ] in
  let short_slice = {
    SM.Slice.msg        = short_msg;
    SM.Slice.segment    = SM.Message.get_segment short_msg 0;
    SM.Slice.segment_id = 0;
    SM.Slice.start      = 0;
    SM.Slice.len        = SM.Message.total_size short_msg;
  } in
  let t0 ctx = assert_equal (SM.Slice.get_int64 short_slice 0)
      0x5a77665544332211L
  in
  let t1 ctx = assert_equal (SM.Slice.get_int64 short_slice 8)
      (-0x5a00112233445567L)
  in
  let t2 ctx = assert_raises
      (Invalid_argument "Slice.get_int64")
      (fun () -> SM.Slice.get_int64 short_slice (-1))
  in
  let t3 ctx = assert_raises
      (Invalid_argument "Slice.get_int64")
      (fun () -> SM.Slice.get_int64 short_slice 9)
  in
  let t4 ctx = assert_raises
      (Invalid_argument "Slice.set_int64")
      (fun () -> SM.Slice.set_int64 short_slice (-1) Int64.zero)
  in
  let t5 ctx = assert_raises
      (Invalid_argument "Slice.set_int64")
      (fun () -> SM.Slice.set_int64 short_slice 9 Int64.zero)
  in
  let t6 ctx =
    let () = SM.Slice.set_int64 short_slice 8 Int64.min_int in
    assert_equal (SM.Slice.get_int64 short_slice 8) Int64.min_int
  in
  let t7 ctx =
    let () = SM.Slice.set_int64 short_slice 0 Int64.max_int in
    assert_equal (SM.Slice.get_int64 short_slice 0) Int64.max_int
  in
  "slice_int64" >::: [
    "get first" >:: t0;
    "get last" >:: t1;
    "get below range" >:: t2;
    "get above range" >:: t3;
    "set below range" >:: t4;
    "set above range" >:: t5;
    "set last" >:: t6;
    "set first" >:: t7;
  ]



let () = run_test_tt_main uint8_suite
let () = run_test_tt_main uint16_suite
let () = run_test_tt_main uint32_suite
let () = run_test_tt_main uint64_suite
let () = run_test_tt_main int8_suite
let () = run_test_tt_main int16_suite
let () = run_test_tt_main int32_suite
let () = run_test_tt_main int64_suite


