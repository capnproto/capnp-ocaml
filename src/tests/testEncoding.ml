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

(* Inspired by encoding-test.c++, as found in the capnproto source. *)

module SM = Capnp.Message.Make(Capnp.StringStorage)
module T  = Test.Make(SM)

open OUnit2


let assert_float_equal f1 f2 eps =
  let f1_abs = abs_float f1 in
  let f2_abs = abs_float f2 in
  let largest = max f1_abs f2_abs in
  let delta = abs_float (f1 -. f2) in
  assert_bool "floating point equality" (delta <= largest *. 3.0 *. eps)

let assert_float32_equal f1 f2 = assert_float_equal f1 f2 1.192092896e-07
let assert_float64_equal f1 f2 = assert_float_equal f1 f2 epsilon_float


let init_test_message (s : T.Builder.TestAllTypes.t) : unit =
  let open T.Builder.TestAllTypes in
  let () = void_field_set s in
  let () = bool_field_set s true in
  let () = int8_field_set_exn s (-123) in
  let () = int16_field_set_exn s (-12345) in
  let () = int32_field_set s (-12345678l) in
  let () = int64_field_set s (-123456789012345L) in
  let () = u_int8_field_set_exn s 234 in
  let () = u_int16_field_set_exn s 45678 in
  let () = u_int32_field_set s (Uint32.of_int 3456789012) in
  let () = u_int64_field_set s (Uint64.of_string "12345678901234567890") in
  let () = float32_field_set s 1234.5 in
  let () = float64_field_set s (-123e45) in
  let () = text_field_set s "foo" in
  let () = data_field_set s "bar" in
  let () =
    let sub = struct_field_init s in
    let () = void_field_set sub in
    let () = bool_field_set sub true in
    let () = int8_field_set_exn sub (-12) in
    let () = int16_field_set_exn sub 3456 in
    let () = int32_field_set sub (-78901234l) in
    let () = int64_field_set sub 56789012345678L in
    let () = u_int8_field_set_exn sub 90 in
    let () = u_int16_field_set_exn sub 1234 in
    let () = u_int32_field_set sub (Uint32.of_int 56789012) in
    let () = u_int64_field_set sub (Uint64.of_string "345678901234567890") in
    let () = float32_field_set sub (-1.25e-10) in
    let () = float64_field_set sub 345.0 in
    let () = text_field_set sub "baz" in
    let () = data_field_set sub "qux" in
    let () =
      let sub_sub = struct_field_init sub in
      let () = text_field_set sub_sub "nested" in
      text_field_set (struct_field_init sub_sub) "really nested"
    in
    let () = enum_field_set sub T.Builder.TestEnum.Baz in

    let _ = void_list_set_list sub [ (); (); () ] in
    let _ = bool_list_set_array sub [| false; true; false; true; true |] in
    let _ = int8_list_set_list sub [ 12; -34; -0x80; 0x7f ] in
    let _ = int16_list_set_list sub [ 1234; -5678; -0x8000; 0x7fff ] in
    let _ = int32_list_set_list sub [ 12345678l; -90123456l; -0x80000000l; 0x7fffffffl ] in
    let _ = int64_list_set_list sub
        [ 123456789012345L; -678901234567890L; -0x8000000000000000L; 0x7fffffffffffffffL ]
    in
    let _ = u_int8_list_set_list sub [ 12; 34; 0; 0xff ] in
    let _ = u_int16_list_set_list sub [ 1234; 5678; 0; 0xffff ] in
    let _ = u_int32_list_set_list sub [
        Uint32.of_int 12345678; Uint32.of_int 90123456;
        Uint32.of_int 0; Uint32.of_string "0xffffffff";
      ]
    in
    let _ = u_int64_list_set_list sub [
        Uint64.of_string "123456789012345"; Uint64.of_string "678901234567890";
        Uint64.zero; Uint64.of_string "0xffffffffffffffff";
      ]
    in
    let _ = float32_list_set_list sub [ 0.0; 1234567.0; 1e37; -1e37; 1e-37; -1e-37 ] in
    let _ = float64_list_set_list sub
        [ 0.0; 123456789012345.0; 1e306; -1e306; 1e-306; -1e-306 ]
    in
    let _ = text_list_set_list sub [ "quux"; "corge"; "grault" ] in
    let _ = data_list_set_list sub [ "garply"; "waldo"; "fred" ] in
    let () =
      let list_builder = struct_list_init sub 3 in
      let () = text_field_set (Capnp.Array.get list_builder 0) "x structlist 1" in
      let () = text_field_set (Capnp.Array.get list_builder 1) "x structlist 2" in
      let () = text_field_set (Capnp.Array.get list_builder 2) "x structlist 3" in
      ()
    in
    let _ = enum_list_set_list sub
        [ T.Builder.TestEnum.Qux; T.Builder.TestEnum.Bar; T.Builder.TestEnum.Grault ]
    in
    ()
  in
  let () = enum_field_set s T.Builder.TestEnum.Corge in

  let _ = void_list_init s 6 in
  let _ = bool_list_set_list s [ true; false; false; true ] in
  let _ = int8_list_set_array s [| 111; -111 |] in
  let _ = int16_list_set_list s [ 11111; -11111 ] in
  let _ = int32_list_set_list s [ 111111111l; -111111111l ] in
  let _ = int64_list_set_list s [ 1111111111111111111L; -1111111111111111111L ] in
  let _ = u_int8_list_set_list s [ 111; 222 ] in
  let _ = u_int16_list_set_list s [ 33333; 44444 ] in
  let _ = u_int32_list_set_list s [ Uint32.of_int 3333333333 ] in
  let _ = u_int64_list_set_list s [ Uint64.of_string "11111111111111111111" ] in
  let _ = float32_list_set_list s [ 5555.5; infinity; neg_infinity; nan ] in
  let _ = float64_list_set_list s [ 7777.75; infinity; neg_infinity; nan ] in
  let _ = text_list_set_list s [ "plugh"; "xyzzy"; "thud" ] in
  let _ = data_list_set_list s [ "oops"; "exhausted"; "rfc3092" ] in
  let () =
    let list_builder = struct_list_init s 3 in
    let _ = text_field_set (Capnp.Array.get list_builder 0) "structlist 1" in
    let _ = text_field_set (Capnp.Array.get list_builder 1) "structlist 2" in
    let _ = text_field_set (Capnp.Array.get list_builder 2) "structlist 3" in
    ()
  in
  let _ = enum_list_set_list s [ T.Builder.TestEnum.Foo; T.Builder.TestEnum.Garply ] in
  ()


let check_test_message (s : T.Reader.TestAllTypes.t) : unit =
  let open T.Reader.TestAllTypes in
  let () = assert_equal () (void_field_get s) in
  let () = assert_equal true (bool_field_get s) in
  let () = assert_equal (-123) (int8_field_get s) in
  let () = assert_equal (-12345) (int16_field_get s) in
  let () = assert_equal (-12345678l) (int32_field_get s) in
  let () = assert_equal (-123456789012345L) (int64_field_get s) in
  let () = assert_equal 234 (u_int8_field_get s) in
  let () = assert_equal 45678 (u_int16_field_get s) in
  let () = assert_equal (Uint32.of_string "3456789012") (u_int32_field_get s) in
  let () = assert_equal (Uint64.of_string "12345678901234567890") (u_int64_field_get s) in
  let () = assert_float32_equal 1234.5 (float32_field_get s) in
  let () = assert_float64_equal (-123e45) (float64_field_get s) in
  let () = assert_equal "foo" (text_field_get s) in
  let () = assert_equal "bar" (data_field_get s) in
  let () = 
    let sub = struct_field_get s in
    let () = assert_equal () (void_field_get sub) in
    let () = assert_equal true (bool_field_get sub) in
    let () = assert_equal (-12) (int8_field_get sub) in
    let () = assert_equal 3456 (int16_field_get sub) in
    let () = assert_equal (-78901234l) (int32_field_get sub) in
    let () = assert_equal 56789012345678L (int64_field_get sub) in
    let () = assert_equal 90 (u_int8_field_get sub) in
    let () = assert_equal 1234 (u_int16_field_get sub) in
    let () = assert_equal (Uint32.of_int 56789012) (u_int32_field_get sub) in
    let () = assert_equal (Uint64.of_string "345678901234567890") (u_int64_field_get sub) in
    let () = assert_float32_equal (-1.25e-10) (float32_field_get sub) in
    let () = assert_float64_equal 345.0 (float64_field_get sub) in
    let () = assert_equal "baz" (text_field_get sub) in
    let () = assert_equal "qux" (data_field_get sub) in
    let () =
      let sub_sub = struct_field_get sub in
      let () = assert_equal "nested" (text_field_get sub_sub) in
      let () = assert_equal "really nested" (text_field_get (struct_field_get sub_sub)) in
      ()
    in
    let () = assert_equal T.Reader.TestEnum.Baz (enum_field_get sub) in
    let () = assert_equal
        [ (); (); () ]
        (void_list_get_list sub)
    in
    let () = assert_equal
        [ false; true; false; true; true ]
        (bool_list_get_list sub)
    in
    let () = assert_equal
        [ 12; -34; -0x80; 0x7f ]
        (int8_list_get_list sub)
    in
    let () = assert_equal
        [ 1234; -5678; -0x8000; 0x7fff ]
        (int16_list_get_list sub)
    in
    let () = assert_equal
        [12345678l; -90123456l; -0x80000000l; 0x7fffffffl]
        (int32_list_get_list sub)
    in
    let () = assert_equal
        [ 123456789012345L; -678901234567890L; -0x8000000000000000L; 0x7fffffffffffffffL ]
        (int64_list_get_list sub)
    in
    let () = assert_equal
        [12; 34; 0; 0xff]
        (u_int8_list_get_list sub)
    in
    let () = assert_equal
        [ 1234; 5678; 0; 0xffff ]
        (u_int16_list_get_list sub)
    in
    let () = assert_equal
        [ Uint32.of_string "12345678"; Uint32.of_string "90123456";
          Uint32.zero; Uint32.of_string "0xffffffff" ]
        (u_int32_list_get_list sub)
    in
    let () = assert_equal
        [ Uint64.of_string "123456789012345"; Uint64.of_string "678901234567890";
          Uint64.zero; Uint64.of_string "0xffffffffffffffff" ]
        (u_int64_list_get_list sub)
    in
    let () = assert_equal
        [ "quux"; "corge"; "grault" ]
        (text_list_get_list sub)
    in
    let () = assert_equal
        [ "garply"; "waldo"; "fred" ]
        (data_list_get_list sub)
    in
    let () =
      let list_reader = struct_list_get sub in
      let () = assert_equal 3 (Capnp.Array.length list_reader) in
      let () = assert_equal "x structlist 1"
          (text_field_get (Capnp.Array.get list_reader 0))
      in
      let () = assert_equal "x structlist 2"
          (text_field_get (Capnp.Array.get list_reader 1))
      in
      let () = assert_equal "x structlist 3"
          (text_field_get (Capnp.Array.get list_reader 2))
      in
      ()
    in
    let () = assert_equal
        [ T.Reader.TestEnum.Qux; T.Reader.TestEnum.Bar;
          T.Reader.TestEnum.Grault ]
        (enum_list_get_list sub)
    in
    ()
  in
  let () = assert_equal 6 (Capnp.Array.length (void_list_get s)) in
  let () = assert_equal
      [ true; false; false; true ]
      (bool_list_get_list s)
  in
  let () = assert_equal
      [ 111; -111 ]
      (int8_list_get_list s)
  in
  let () = assert_equal
      [ 11111; -11111 ]
      (int16_list_get_list s)
  in
  let () = assert_equal
      [ 111111111l; -111111111l ]
      (int32_list_get_list s)
  in
  let () = assert_equal 
      [ 1111111111111111111L; -1111111111111111111L ]
      (int64_list_get_list s)
  in
  let () = assert_equal
      [ 111; 222 ]
      (u_int8_list_get_list s)
  in
  let () = assert_equal
      [ 33333; 44444 ]
      (u_int16_list_get_list s)
  in
  let () = assert_equal
      [ Uint32.of_string "3333333333" ]
      (u_int32_list_get_list s)
  in
  let () = assert_equal
      [ Uint64.of_string "11111111111111111111" ]
      (u_int64_list_get_list s)
  in
  let () =
    let list_reader = float32_list_get s in
    let () = assert_equal 4 (Capnp.Array.length list_reader) in
    let () = assert_float32_equal 5555.5 (Capnp.Array.get list_reader 0) in
    let () = assert_equal infinity (Capnp.Array.get list_reader 1) in
    let () = assert_equal neg_infinity (Capnp.Array.get list_reader 2) in
    let () = assert_equal (Pervasives.compare nan (Capnp.Array.get list_reader 3)) 0 in
    ()
  in
  let () =
    let list_reader = float64_list_get s in
    let () = assert_equal 4 (Capnp.Array.length list_reader) in
    let () = assert_float64_equal 7777.75 (Capnp.Array.get list_reader 0) in
    let () = assert_equal infinity (Capnp.Array.get list_reader 1) in
    let () = assert_equal neg_infinity (Capnp.Array.get list_reader 2) in
    let () = assert_equal (Pervasives.compare nan (Capnp.Array.get list_reader 3)) 0 in
    ()
  in
  let () = assert_equal [ "plugh"; "xyzzy"; "thud" ] (text_list_get_list s) in
  let () = assert_equal [ "oops"; "exhausted"; "rfc3092" ] (data_list_get_list s) in
  let () =
    let list_reader = struct_list_get s in
    let () = assert_equal 3 (Capnp.Array.length list_reader) in
    let () = assert_equal "structlist 1" (text_field_get (Capnp.Array.get list_reader 0)) in
    let () = assert_equal "structlist 2" (text_field_get (Capnp.Array.get list_reader 1)) in
    let () = assert_equal "structlist 3" (text_field_get (Capnp.Array.get list_reader 2)) in
    ()
  in
  let () = assert_equal
      [ T.Reader.TestEnum.Foo; T.Reader.TestEnum.Garply ]
      (enum_list_get_list s)
  in
  ()


let test_encode_decode ctx =
  let message =
    let builder = T.Builder.TestAllTypes.init_root () in
    let () = init_test_message builder in
    T.Builder.TestAllTypes.to_message builder
  in
  let reader = T.Reader.TestAllTypes.of_message message in
  check_test_message reader


let encoding_suite =
  "all_types" >::: [
    "encode/decode" >:: test_encode_decode;
  ]

let () = run_test_tt_main encoding_suite

