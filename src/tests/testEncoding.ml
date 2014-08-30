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

open Core.Std

module BM  = Capnp.BytesMessage
module T   = Test.Make(BM)
module TL  = TestLists.Make(BM)
module TI  = Test_import.Make(BM)

open OUnit2


let assert_float_equal f1 f2 eps =
  let f1_abs = Float.abs f1 in
  let f2_abs = Float.abs f2 in
  let largest = max f1_abs f2_abs in
  let delta = Float.abs (f1 -. f2) in
  assert_bool "floating point equality" (delta <= largest *. 3.0 *. eps)

let assert_float32_equal f1 f2 = assert_float_equal f1 f2 1.192092896e-07
let assert_float64_equal f1 f2 = assert_float_equal f1 f2 Float.epsilon_float

let assert_raises_invalid_arg f =
  try
    let _ = f () in
    assert_failure "did not raise Invalid_argument"
  with Invalid_argument _ ->
    ()

let assert_raises_out_of_int_range f =
  try
    let _ = f () in
    assert_failure "did not raise Out_of_int_range"
  with Capnp.Message.Out_of_int_range _ ->
    ()


let init_test_message (s : T.Builder.TestAllTypes.t) : unit =
  let open T.Builder.TestAllTypes in
  void_field_set s;
  bool_field_set s true;
  int8_field_set_exn s (-123);
  int16_field_set_exn s (-12345);
  int32_field_set s (-12345678l);
  int64_field_set s (-123456789012345L);
  u_int8_field_set_exn s 234;
  u_int16_field_set_exn s 45678;
  u_int32_field_set s (Uint32.of_int 3456789012);
  u_int64_field_set s (Uint64.of_string "12345678901234567890");
  float32_field_set s 1234.5;
  float64_field_set s (-123e45);
  text_field_set s "foo";
  data_field_set s "bar";
  let () =
    let sub = struct_field_init s in
    void_field_set sub;
    bool_field_set sub true;
    int8_field_set_exn sub (-12);
    int16_field_set_exn sub 3456;
    int32_field_set sub (-78901234l);
    int64_field_set sub 56789012345678L;
    u_int8_field_set_exn sub 90;
    u_int16_field_set_exn sub 1234;
    u_int32_field_set sub (Uint32.of_int 56789012);
    u_int64_field_set sub (Uint64.of_string "345678901234567890");
    float32_field_set sub (-1.25e-10);
    float64_field_set sub 345.0;
    text_field_set sub "baz";
    data_field_set sub "qux";
    let () =
      let sub_sub = struct_field_init sub in
      text_field_set sub_sub "nested";
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
      text_field_set (Capnp.Array.get list_builder 0) "x structlist 1";
      text_field_set (Capnp.Array.get list_builder 1) "x structlist 2";
      text_field_set (Capnp.Array.get list_builder 2) "x structlist 3"
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
  let _ = float32_list_set_list s [ 5555.5; Float.infinity; Float.neg_infinity; Float.nan ] in
  let _ = float64_list_set_list s [ 7777.75; Float.infinity; Float.neg_infinity; Float.nan ] in
  let _ = text_list_set_list s [ "plugh"; "xyzzy"; "thud" ] in
  let _ = data_list_set_list s [ "oops"; "exhausted"; "rfc3092" ] in
  let () =
    let list_builder = struct_list_init s 3 in
    text_field_set (Capnp.Array.get list_builder 0) "structlist 1";
    text_field_set (Capnp.Array.get list_builder 1) "structlist 2";
    text_field_set (Capnp.Array.get list_builder 2) "structlist 3"
  in
  let _ = enum_list_set_list s [ T.Builder.TestEnum.Foo; T.Builder.TestEnum.Garply ] in
  ()


(* Provide a signature for the TestAllTypes module which we can implement
   using either a Reader or a Builder. *)
module type TEST_ALL_TYPES = sig
  type t
  type array_t
  type 'a message_t
  type access
  type message_access
  type inner_struct_t

  val void_field_get : t -> unit
  val bool_field_get : t -> bool
  val int8_field_get : t -> int
  val int16_field_get : t -> int
  val int32_field_get : t -> int32
  val int32_field_get_int_exn : t -> int
  val int64_field_get : t -> int64
  val int64_field_get_int_exn : t -> int
  val u_int8_field_get : t -> int
  val u_int16_field_get : t -> int
  val u_int32_field_get : t -> Uint32.t
  val u_int32_field_get_int_exn : t -> int
  val u_int64_field_get : t -> Uint64.t
  val u_int64_field_get_int_exn : t -> int
  val float32_field_get : t -> float
  val float64_field_get : t -> float
  val has_text_field : t -> bool
  val text_field_get : t -> string
  val has_data_field : t -> bool
  val data_field_get : t -> string
  val has_struct_field : t -> bool
  val struct_field_get : t -> inner_struct_t
  val enum_field_get : t -> T.Reader.TestEnum.t
  val interface_field_get : t -> unit
  val has_void_list : t -> bool
  val void_list_get : t -> (access, unit, array_t) Capnp.Array.t
  val void_list_get_list : t -> unit list
  val void_list_get_array : t -> unit array
  val has_bool_list : t -> bool
  val bool_list_get : t -> (access, bool, array_t) Capnp.Array.t
  val bool_list_get_list : t -> bool list
  val bool_list_get_array : t -> bool array
  val has_int8_list : t -> bool
  val int8_list_get : t -> (access, int, array_t) Capnp.Array.t
  val int8_list_get_list : t -> int list
  val int8_list_get_array : t -> int array
  val has_int16_list : t -> bool
  val int16_list_get : t -> (access, int, array_t) Capnp.Array.t
  val int16_list_get_list : t -> int list
  val int16_list_get_array : t -> int array
  val has_int32_list : t -> bool
  val int32_list_get : t -> (access, int32, array_t) Capnp.Array.t
  val int32_list_get_list : t -> int32 list
  val int32_list_get_array : t -> int32 array
  val has_int64_list : t -> bool
  val int64_list_get : t -> (access, int64, array_t) Capnp.Array.t
  val int64_list_get_list : t -> int64 list
  val int64_list_get_array : t -> int64 array
  val has_u_int8_list : t -> bool
  val u_int8_list_get : t -> (access, int, array_t) Capnp.Array.t
  val u_int8_list_get_list : t -> int list
  val u_int8_list_get_array : t -> int array
  val has_u_int16_list : t -> bool
  val u_int16_list_get : t -> (access, int, array_t) Capnp.Array.t
  val u_int16_list_get_list : t -> int list
  val u_int16_list_get_array : t -> int array
  val has_u_int32_list : t -> bool
  val u_int32_list_get : t -> (access, Uint32.t, array_t) Capnp.Array.t
  val u_int32_list_get_list : t -> Uint32.t list
  val u_int32_list_get_array : t -> Uint32.t array
  val has_u_int64_list : t -> bool
  val u_int64_list_get : t -> (access, Uint64.t, array_t) Capnp.Array.t
  val u_int64_list_get_list : t -> Uint64.t list
  val u_int64_list_get_array : t -> Uint64.t array
  val has_float32_list : t -> bool
  val float32_list_get : t -> (access, float, array_t) Capnp.Array.t
  val float32_list_get_list : t -> float list
  val float32_list_get_array : t -> float array
  val has_float64_list : t -> bool
  val float64_list_get : t -> (access, float, array_t) Capnp.Array.t
  val float64_list_get_list : t -> float list
  val float64_list_get_array : t -> float array
  val has_text_list : t -> bool
  val text_list_get : t -> (access, string, array_t) Capnp.Array.t
  val text_list_get_list : t -> string list
  val text_list_get_array : t -> string array
  val has_data_list : t -> bool
  val data_list_get : t -> (access, string, array_t) Capnp.Array.t
  val data_list_get_list : t -> string list
  val data_list_get_array : t -> string array
  val has_struct_list : t -> bool
  val struct_list_get : t -> (access, inner_struct_t, array_t) Capnp.Array.t
  val struct_list_get_list : t -> inner_struct_t list
  val struct_list_get_array : t -> inner_struct_t array
  val has_enum_list : t -> bool
  val enum_list_get : t -> (access, T.Reader.TestEnum.t, array_t) Capnp.Array.t
  val enum_list_get_list : t -> T.Reader.TestEnum.t list
  val enum_list_get_array : t -> T.Reader.TestEnum.t array
  val has_interface_list : t -> bool
  val interface_list_get : t -> (access, unit, array_t) Capnp.Array.t
  val interface_list_get_list : t -> unit list
  val interface_list_get_array : t -> unit array
  val of_message : message_access message_t -> t
end


(* Using a two-argument functor here because the Outer module may be either
   TestAllTypes or TestDefaults (i.e. completely different structs defined in the
   schema which happen to share the same structure), but the Inner module (obtained
   via [struct_field_get] or [struct_list_get] is always a variant of TestAllTypes
   due to the way the schema was defined. *)

module Check_test_message
    (Outer : TEST_ALL_TYPES)
    (Inner : TEST_ALL_TYPES
     with type t = Outer.inner_struct_t
      and type inner_struct_t = Outer.inner_struct_t)
= struct

  let f (s : Outer.t) : unit =
    let open Outer in
    assert_equal () (void_field_get s);
    assert_equal true (bool_field_get s);
    assert_equal (-123) (int8_field_get s);
    assert_equal (-12345) (int16_field_get s);
    assert_equal (-12345678l) (int32_field_get s);
    assert_equal (-123456789012345L) (int64_field_get s);
    assert_equal 234 (u_int8_field_get s);
    assert_equal 45678 (u_int16_field_get s);
    assert_equal (Uint32.of_string "3456789012") (u_int32_field_get s);
    assert_equal (Uint64.of_string "12345678901234567890") (u_int64_field_get s);
    assert_float32_equal 1234.5 (float32_field_get s);
    assert_float64_equal (-123e45) (float64_field_get s);
    assert_equal "foo" (text_field_get s);
    assert_equal "bar" (data_field_get s);
    let () =
      let sub = struct_field_get s in
      let open Inner in
      assert_equal () (void_field_get sub);
      assert_equal true (bool_field_get sub);
      assert_equal (-12) (int8_field_get sub);
      assert_equal 3456 (int16_field_get sub);
      assert_equal (-78901234l) (int32_field_get sub);
      assert_equal 56789012345678L (int64_field_get sub);
      assert_equal 90 (u_int8_field_get sub);
      assert_equal 1234 (u_int16_field_get sub);
      assert_equal (Uint32.of_int 56789012) (u_int32_field_get sub);
      assert_equal (Uint64.of_string "345678901234567890") (u_int64_field_get sub);
      assert_float32_equal (-1.25e-10) (float32_field_get sub);
      assert_float64_equal 345.0 (float64_field_get sub);
      assert_equal "baz" (text_field_get sub);
      assert_equal "qux" (data_field_get sub);
      let () =
        let sub_sub = struct_field_get sub in
        assert_equal "nested" (text_field_get sub_sub);
        assert_equal "really nested" (text_field_get (struct_field_get sub_sub))
      in
      assert_equal T.Reader.TestEnum.Baz (enum_field_get sub);
      assert_equal [ (); (); () ] (void_list_get_list sub);
      assert_equal [ false; true; false; true; true ] (bool_list_get_list sub);
      assert_equal [ 12; -34; -0x80; 0x7f ] (int8_list_get_list sub);
      assert_equal [ 1234; -5678; -0x8000; 0x7fff ] (int16_list_get_list sub);
      assert_equal
        [12345678l; -90123456l; -0x80000000l; 0x7fffffffl]
        (int32_list_get_list sub);
      assert_equal
        [ 123456789012345L; -678901234567890L; -0x8000000000000000L; 0x7fffffffffffffffL ]
        (int64_list_get_list sub);
      assert_equal [12; 34; 0; 0xff] (u_int8_list_get_list sub);
      assert_equal [ 1234; 5678; 0; 0xffff ] (u_int16_list_get_list sub);
      assert_equal
        [ Uint32.of_string "12345678"; Uint32.of_string "90123456";
          Uint32.zero; Uint32.of_string "0xffffffff" ]
        (u_int32_list_get_list sub);
      assert_equal
        [ Uint64.of_string "123456789012345"; Uint64.of_string "678901234567890";
          Uint64.zero; Uint64.of_string "0xffffffffffffffff" ]
        (u_int64_list_get_list sub);
      assert_equal [ "quux"; "corge"; "grault" ] (text_list_get_list sub);
      assert_equal [ "garply"; "waldo"; "fred" ] (data_list_get_list sub);
      let () =
        let list_reader = struct_list_get sub in
        assert_equal 3 (Capnp.Array.length list_reader);
        assert_equal "x structlist 1" (text_field_get (Capnp.Array.get list_reader 0));
        assert_equal "x structlist 2" (text_field_get (Capnp.Array.get list_reader 1));
        assert_equal "x structlist 3" (text_field_get (Capnp.Array.get list_reader 2))
      in
      assert_equal
        [ T.Reader.TestEnum.Qux; T.Reader.TestEnum.Bar;
          T.Reader.TestEnum.Grault ]
        (enum_list_get_list sub)
    in
    assert_equal 6 (Capnp.Array.length (void_list_get s));
    assert_equal [ true; false; false; true ] (bool_list_get_list s);
    assert_equal [ 111; -111 ] (int8_list_get_list s);
    assert_equal [ 11111; -11111 ] (int16_list_get_list s);
    assert_equal [ 111111111l; -111111111l ] (int32_list_get_list s);
    assert_equal [ 1111111111111111111L; -1111111111111111111L ] (int64_list_get_list s);
    assert_equal [ 111; 222 ] (u_int8_list_get_list s);
    assert_equal [ 33333; 44444 ] (u_int16_list_get_list s);
    assert_equal [ Uint32.of_string "3333333333" ] (u_int32_list_get_list s);
    assert_equal [ Uint64.of_string "11111111111111111111" ] (u_int64_list_get_list s);
    let () =
      let list_reader = float32_list_get s in
      assert_equal 4 (Capnp.Array.length list_reader);
      assert_float32_equal 5555.5 (Capnp.Array.get list_reader 0);
      assert_equal Float.infinity (Capnp.Array.get list_reader 1);
      assert_equal Float.neg_infinity (Capnp.Array.get list_reader 2);
      assert_equal (Pervasives.compare Float.nan (Capnp.Array.get list_reader 3)) 0
    in
    let () =
      let list_reader = float64_list_get s in
      assert_equal 4 (Capnp.Array.length list_reader);
      assert_float64_equal 7777.75 (Capnp.Array.get list_reader 0);
      assert_equal Float.infinity (Capnp.Array.get list_reader 1);
      assert_equal Float.neg_infinity (Capnp.Array.get list_reader 2);
      assert_equal (Pervasives.compare Float.nan (Capnp.Array.get list_reader 3)) 0
    in
    assert_equal [ "plugh"; "xyzzy"; "thud" ] (text_list_get_list s);
    assert_equal [ "oops"; "exhausted"; "rfc3092" ] (data_list_get_list s);
    let () =
      let list_reader = struct_list_get s in
      let open Inner in
      assert_equal 3 (Capnp.Array.length list_reader);
      assert_equal "structlist 1" (text_field_get (Capnp.Array.get list_reader 0));
      assert_equal "structlist 2" (text_field_get (Capnp.Array.get list_reader 1));
      assert_equal "structlist 3" (text_field_get (Capnp.Array.get list_reader 2))
    in
    assert_equal [ T.Reader.TestEnum.Foo; T.Reader.TestEnum.Garply ] (enum_list_get_list s)
end

module ReaderTestAllTypes = struct
  include T.Reader.TestAllTypes
  type 'a message_t = 'a T.message_t
  type array_t = T.Reader.array_t
  type access = Test.ro
  type message_access = Test.ro
  type inner_struct_t = t
end

module Reader_check_test_message =
  Check_test_message(ReaderTestAllTypes)(ReaderTestAllTypes)

module BuilderTestAllTypes = struct
  include T.Builder.TestAllTypes
  type 'a message_t = 'a T.message_t
  type array_t = T.Builder.array_t
  type access = Test.rw
  type message_access = Test.rw
  type inner_struct_t = t
end

module Builder_check_test_message =
  Check_test_message(BuilderTestAllTypes)(BuilderTestAllTypes)

module ReaderTestDefaults = struct
  include T.Reader.TestDefaults
  type 'a message_t = 'a T.message_t
  type array_t = T.Reader.array_t
  type access = Test.ro
  type message_access = Test.ro
  type inner_struct_t = T.Reader.TestAllTypes.t
end

module Reader_check_test_defaults =
  Check_test_message(ReaderTestDefaults)(ReaderTestAllTypes)

module BuilderTestDefaults = struct
  include T.Builder.TestDefaults
  type 'a message_t = 'a T.message_t
  type array_t = T.Builder.array_t
  type access = Test.rw
  type message_access = Test.rw
  type inner_struct_t = T.Builder.TestAllTypes.t
end

module Builder_check_test_defaults =
  Check_test_message(BuilderTestDefaults)(BuilderTestAllTypes)


let test_encode_decode ctx =
  let builder = T.Builder.TestAllTypes.init_root () in
  let () = init_test_message builder in
  let () = Builder_check_test_message.f builder in
  let reader = T.Reader.TestAllTypes.of_builder builder in
  Reader_check_test_message.f reader


let test_decode_defaults ctx =
  let null_root = Bytes.of_string "\x00\x00\x00\x00\x00\x00\x00\x00" in
  let message = BM.Message.readonly (BM.Message.of_storage [ null_root ]) in
  let reader = T.Reader.TestDefaults.of_message message in
  Reader_check_test_defaults.f reader


let test_init_defaults ctx =
  let null_root = Bytes.of_string "\x00\x00\x00\x00\x00\x00\x00\x00" in
  let message = BM.Message.of_storage [ null_root ] in
  let builder = T.Builder.TestDefaults.of_message message in
  (* First pass initializes [message] with defaults *)
  let () = Builder_check_test_defaults.f builder in
  (* Second pass just reads the initialized structure *)
  let () = Builder_check_test_defaults.f builder in
  let reader = T.Reader.TestDefaults.of_builder builder in
  Reader_check_test_defaults.f reader


let test_union_encoding ctx =
  let open T.Builder.TestUnion in
  let root = init_root () in
  let union0 = union0_get root in
  assert_equal Union0.U0f0s0 (Union0.get union0);
  Union0.u0f0s1_set union0 true;
  assert_equal (Union0.U0f0s1 true) (Union0.get union0);
  Union0.u0f0s8_set_exn union0 123;
  assert_equal (Union0.U0f0s8 123) (Union0.get union0)


let init_union (setter : T.Builder.TestUnion.t -> 'a) =
  (* Use the given setter to initialize the given union field and then
     return both the location of the data that was written as well as
     the values of the four union discriminants. *)
  let builder = T.Builder.TestUnion.init_root ~message_size:1024 () in
  let _ = setter builder in
  let message = BM.Message.readonly (T.Builder.TestUnion.to_message builder) in
  let segment = BM.Message.get_segment message 0 in

  (* Find the offset of the first set bit after the union discriminants. *)
  let bit_offset =
    let starting_byte = 16 in
    let rec loop byte_ofs bit_ofs =
      if byte_ofs = BM.Segment.length segment then
        None
      else if bit_ofs = 8 then
        loop (byte_ofs + 1) 0
      else
        let byte_val = BM.Segment.get_uint8 segment byte_ofs in
        if ((1 lsl bit_ofs) land byte_val) <> 0 then
          Some ((8 * (byte_ofs - starting_byte)) + bit_ofs)
        else
          loop byte_ofs (bit_ofs + 1)
    in
    loop starting_byte 0
  in
  ([ BM.Segment.get_uint16 segment 8;
     BM.Segment.get_uint16 segment 10;
     BM.Segment.get_uint16 segment 12;
     BM.Segment.get_uint16 segment 14; ],
   bit_offset)


let test_union_layout ctx =
  let open T.Builder.TestUnion in
  assert_equal ([ 0; 0; 0; 0 ], None)
    (init_union (fun b -> Union0.u0f0s0_set (union0_get b)));
  assert_equal ([ 1; 0; 0; 0 ], Some 0)
    (init_union (fun b -> Union0.u0f0s1_set (union0_get b) true));
  assert_equal ([ 2; 0; 0; 0 ], Some 0)
    (init_union (fun b -> Union0.u0f0s8_set_exn (union0_get b) 1));
  assert_equal ([ 3; 0; 0; 0 ], Some 0)
    (init_union (fun b -> Union0.u0f0s16_set_exn (union0_get b) 1));
  assert_equal ([ 4; 0; 0; 0 ], Some 0)
    (init_union (fun b -> Union0.u0f0s32_set (union0_get b) 1l));
  assert_equal ([ 5; 0; 0; 0 ], Some 0)
    (init_union (fun b -> Union0.u0f0s64_set (union0_get b) 1L));
  assert_equal ([ 6; 0; 0; 0 ], Some 448)
    (init_union (fun b -> Union0.u0f0sp_set (union0_get b) "1"));

  assert_equal ([ 7; 0; 0; 0], None)
    (init_union (fun b -> Union0.u0f1s0_set (union0_get b)));
  assert_equal ([ 8; 0; 0; 0], Some 0)
    (init_union (fun b -> Union0.u0f1s1_set (union0_get b) true));
  assert_equal ([ 9; 0; 0; 0], Some 0)
    (init_union (fun b -> Union0.u0f1s8_set_exn (union0_get b) 1));
  assert_equal ([ 10; 0; 0; 0 ], Some 0)
    (init_union (fun b -> Union0.u0f1s16_set_exn (union0_get b) 1));
  assert_equal ([ 11; 0; 0; 0 ], Some 0)
    (init_union (fun b -> Union0.u0f1s32_set (union0_get b) 1l));
  assert_equal ([ 12; 0; 0; 0 ], Some 0)
    (init_union (fun b -> Union0.u0f1s64_set (union0_get b) 1L));
  assert_equal ([ 13; 0; 0; 0 ], Some 448)
    (init_union (fun b -> Union0.u0f1sp_set (union0_get b) "1"));

  assert_equal ([ 0; 0; 0; 0 ], None)
    (init_union (fun b -> Union1.u1f0s0_set (union1_get b)));
  assert_equal ([ 0; 1; 0; 0 ], Some 65)
    (init_union (fun b -> Union1.u1f0s1_set (union1_get b) true));
  assert_equal ([ 0; 2; 0; 0 ], Some 65)
    (init_union (fun b -> Union1.u1f1s1_set (union1_get b) true));
  assert_equal ([ 0; 3; 0; 0 ], Some 72)
    (init_union (fun b -> Union1.u1f0s8_set_exn (union1_get b) 1));
  assert_equal ([ 0; 4; 0; 0 ], Some 72)
    (init_union (fun b -> Union1.u1f1s8_set_exn (union1_get b) 1));
  assert_equal ([ 0; 5; 0; 0 ], Some 80)
    (init_union (fun b -> Union1.u1f0s16_set_exn (union1_get b) 1));
  assert_equal ([ 0; 6; 0; 0 ], Some 80)
    (init_union (fun b -> Union1.u1f1s16_set_exn (union1_get b) 1));
  assert_equal ([ 0; 7; 0; 0 ], Some 96)
    (init_union (fun b -> Union1.u1f0s32_set (union1_get b) 1l));
  assert_equal ([ 0; 8; 0; 0 ], Some 96)
    (init_union (fun b -> Union1.u1f1s32_set (union1_get b) 1l));
  assert_equal ([ 0; 9; 0; 0 ], Some 128)
    (init_union (fun b -> Union1.u1f0s64_set (union1_get b) 1L));
  assert_equal ([ 0; 10; 0; 0 ], Some 128)
    (init_union (fun b -> Union1.u1f1s64_set (union1_get b) 1L));
  assert_equal ([ 0; 11; 0; 0 ], Some 512)
    (init_union (fun b -> Union1.u1f0sp_set (union1_get b) "1"));
  assert_equal ([ 0; 12; 0; 0 ], Some 512)
    (init_union (fun b -> Union1.u1f1sp_set (union1_get b) "1"));

  assert_equal ([ 0; 13; 0; 0 ], None)
    (init_union (fun b -> Union1.u1f2s0_set (union1_get b)));
  assert_equal ([ 0; 14; 0; 0 ], Some 65)
    (init_union (fun b -> Union1.u1f2s1_set (union1_get b) true));
  assert_equal ([ 0; 15; 0; 0 ], Some 72)
    (init_union (fun b -> Union1.u1f2s8_set_exn (union1_get b) 1));
  assert_equal ([ 0; 16; 0; 0 ], Some 80)
    (init_union (fun b -> Union1.u1f2s16_set_exn (union1_get b) 1));
  assert_equal ([ 0; 17; 0; 0 ], Some 96)
    (init_union (fun b -> Union1.u1f2s32_set (union1_get b) 1l));
  assert_equal ([ 0; 18; 0; 0 ], Some 128)
    (init_union (fun b -> Union1.u1f2s64_set (union1_get b) 1L));
  assert_equal ([ 0; 19; 0; 0 ], Some 512)
    (init_union (fun b -> Union1.u1f2sp_set (union1_get b) "1"));

  assert_equal ([ 0; 0; 0; 0 ], Some 192)
    (init_union (fun b -> Union2.u2f0s1_set (union2_get b) true));
  assert_equal ([ 0; 0; 0; 0 ], Some 193)
    (init_union (fun b -> Union3.u3f0s1_set (union3_get b) true));
  assert_equal ([ 0; 0; 1; 0 ], Some 200)
    (init_union (fun b -> Union2.u2f0s8_set_exn (union2_get b) 1));
  assert_equal ([ 0; 0; 0; 1 ], Some 208)
    (init_union (fun b -> Union3.u3f0s8_set_exn (union3_get b) 1));
  assert_equal ([ 0; 0; 2; 0 ], Some 224)
    (init_union (fun b -> Union2.u2f0s16_set_exn (union2_get b) 1));
  assert_equal ([ 0; 0; 0; 2 ], Some 240)
    (init_union (fun b -> Union3.u3f0s16_set_exn (union3_get b) 1));
  assert_equal ([ 0; 0; 3; 0 ], Some 256)
    (init_union (fun b -> Union2.u2f0s32_set (union2_get b) 1l));
  assert_equal ([ 0; 0; 0; 3 ], Some 288)
    (init_union (fun b -> Union3.u3f0s32_set (union3_get b) 1l));
  assert_equal ([ 0; 0; 4; 0 ], Some 320)
    (init_union (fun b -> Union2.u2f0s64_set (union2_get b) 1L));
  assert_equal ([ 0; 0; 0; 4 ], Some 384)
    (init_union (fun b -> Union3.u3f0s64_set (union3_get b) 1L))


let test_unnamed_union_encoding ctx =
  let module R = T.Reader.TestUnnamedUnion in
  let module B = T.Builder.TestUnnamedUnion in
  let root = B.init_root () in
  assert_equal (B.Foo 0) (B.get root);

  B.bar_set_int_exn root 321;
  assert_equal (B.Bar (Uint32.of_int 321)) (B.get root);
  assert_equal (R.Bar (Uint32.of_int 321)) (R.get (R.of_builder root));

  B.foo_set_exn root 123;
  assert_equal (B.Foo 123) (B.get root);
  assert_equal (R.Foo 123) (R.get (B.to_reader root))


let test_groups ctx =
  let open T.Builder.TestGroups in
  let root = init_root () in
  let () =
    let groups = groups_get root in
    let foo = Groups.foo_init groups in
    Groups.Foo.corge_set foo 12345678l;
    Groups.Foo.grault_set foo 123456789012345L;
    Groups.Foo.garply_set foo "foobar";

    assert_equal 12345678l (Groups.Foo.corge_get foo);
    assert_equal 123456789012345L (Groups.Foo.grault_get foo);
    assert_equal "foobar" (Groups.Foo.garply_get foo)
  in
  let () =
    let groups = groups_get root in
    let bar = Groups.bar_init groups in
    Groups.Bar.corge_set bar 23456789l;
    Groups.Bar.grault_set bar "bazbaz";
    Groups.Bar.garply_set bar 234567890123456L;

    assert_equal 23456789l (Groups.Bar.corge_get bar);
    assert_equal "bazbaz" (Groups.Bar.grault_get bar);
    assert_equal 234567890123456L (Groups.Bar.garply_get bar)
  in
  let () =
    let groups = groups_get root in
    let baz = Groups.baz_init groups in
    Groups.Baz.corge_set baz 34567890l;
    Groups.Baz.grault_set baz "bazqux";
    Groups.Baz.garply_set baz "quxquux";

    assert_equal 34567890l (Groups.Baz.corge_get baz);
    assert_equal "bazqux" (Groups.Baz.grault_get baz);
    assert_equal "quxquux" (Groups.Baz.garply_get baz)
  in
  ()


let test_interleaved_groups ctx =
  let module B = T.Builder.TestInterleavedGroups in
  let module R = T.Reader.TestInterleavedGroups in
  let root = B.init_root () in

  (* Init both groups to different values. *)
  let () =
    let group = B.group1_get root in
    B.Group1.foo_set_int_exn group 12345678;
    B.Group1.bar_set group (Uint64.of_string "123456789012345");
    let corge = B.Group1.corge_init group in
    B.Group1.Corge.grault_set corge (Uint64.of_string "987654321098765");
    B.Group1.Corge.garply_set_exn corge 12345;
    B.Group1.Corge.plugh_set corge "plugh";
    B.Group1.Corge.xyzzy_set corge "xyzzy";
    B.Group1.waldo_set group "waldo"
  in
  let () =
    let group = B.group2_get root in
    B.Group2.foo_set_int_exn group 23456789;
    B.Group2.bar_set group (Uint64.of_string "234567890123456");
    let corge = B.Group2.corge_init group in
    B.Group2.Corge.grault_set corge (Uint64.of_string "876543210987654");
    B.Group2.Corge.garply_set_exn corge 23456;
    B.Group2.Corge.plugh_set corge "hgulp";
    B.Group2.Corge.xyzzy_set corge "yzzyx";
    B.Group2.waldo_set group "odlaw"
  in

  (* Verify that group 1 is still set correctly. *)
  let () =
    let group = R.group1_get (R.of_builder root) in
    assert_equal 12345678 (R.Group1.foo_get_int_exn group);
    assert_equal (Uint64.of_string "123456789012345") (R.Group1.bar_get group);
    match R.Group1.get group with
    | R.Group1.Corge corge ->
        assert_equal (Uint64.of_string "987654321098765") (R.Group1.Corge.grault_get corge);
        assert_equal 12345 (R.Group1.Corge.garply_get corge);
        assert_equal "plugh" (R.Group1.Corge.plugh_get corge);
        assert_equal "xyzzy" (R.Group1.Corge.xyzzy_get corge);
        let _ = R.Group1.has_waldo group in
        assert_equal "waldo" (R.Group1.waldo_get group)
    | _ ->
        assert_failure "Corge unexpectedly unset"
  in

  (* Zero out group 1 and verify that it is zero'd *)
  let () =
    let group = R.Group1.of_builder (B.group1_init root) in
    assert_equal Uint32.zero (R.Group1.foo_get group);
    assert_equal Uint64.zero (R.Group1.bar_get group);
    let () =
      match R.Group1.get group with
      | R.Group1.Qux x ->
          assert_equal 0 x
      | _ ->
          assert_failure "Qux unexpectedly unset"
    in
    assert_equal false (R.Group1.has_waldo group)
  in

  (* Group 2 should not have been touched *)
  let () =
    let group = R.group2_get (R.of_builder root) in
    assert_equal 23456789 (R.Group2.foo_get_int_exn group);
    assert_equal (Uint64.of_string "234567890123456") (R.Group2.bar_get group);
    match R.Group2.get group with
    | R.Group2.Corge corge ->
        assert_equal (Uint64.of_string "876543210987654") (R.Group2.Corge.grault_get corge);
        assert_equal 23456 (R.Group2.Corge.garply_get corge);
        assert_equal "hgulp" (R.Group2.Corge.plugh_get corge);
        assert_equal "yzzyx" (R.Group2.Corge.xyzzy_get corge);
        assert_equal "odlaw" (R.Group2.waldo_get group)
    | _ ->
        assert_failure "Corge unexpectedly unset"
  in
  ()


let test_union_defaults ctx =
  let module B = T.Builder.TestUnionDefaults in
  let module R = T.Reader.TestUnionDefaults in
  let reader = R.of_builder (B.init_root ()) in
  (* Note: the following code is pretty clumsy.  Seems like a getter for a named union
     field could just bypass the intermediate type? *)
  let () =
    let field = R.s16s8s64s8_set_get reader in
    let module TU = T.Reader.TestUnion in
    begin match TU.Union0.get (TU.union0_get field) with
    | TU.Union0.U0f0s16 321 ->
        ()
    | _ ->
        assert_failure "bad union0 default"
    end;
    begin match TU.Union1.get (TU.union1_get field) with
    | TU.Union1.U1f0s8 123 ->
        ()
    | _ ->
        assert_failure "bad union1 default"
    end;
    begin match TU.Union2.get (TU.union2_get field) with
    | TU.Union2.U2f0s64 12345678901234567L ->
        ()
    | _ ->
        assert_failure "bad union2 default"
    end;
    begin match TU.Union3.get (TU.union3_get field) with
    | TU.Union3.U3f0s8 55 ->
        ()
    | _ ->
        assert_failure "bad union3 default"
    end
  in
  let () =
    let field = R.s0sps1s32_set_get reader in
    let module TU = T.Reader.TestUnion in
    begin match TU.Union0.get (TU.union0_get field) with
    | TU.Union0.U0f1s0 ->
        ()
    | _ ->
        assert_failure "bad union0 default"
    end;
    begin match TU.Union1.get (TU.union1_get field) with
    | TU.Union1.U1f0sp s when s = "foo" ->
        ()
    | _ ->
        assert_failure "bad union1 default"
    end;
    begin match TU.Union2.get (TU.union2_get field) with
    | TU.Union2.U2f0s1 true ->
        ()
    | _ ->
        assert_failure "bad union2 default"
    end;
    begin match TU.Union3.get (TU.union3_get field) with
    | TU.Union3.U3f0s32 12345678l ->
        ()
    | _ -> assert_failure "bad union3 default"
    end
  in
  let () =
    let field = R.unnamed1_get reader in
    begin match T.Reader.TestUnnamedUnion.get field with
    | T.Reader.TestUnnamedUnion.Foo 123 ->
        ()
    | _ -> assert_failure "bad unnamed1 default"
    end;
    assert_equal false (T.Reader.TestUnnamedUnion.has_before field);
    assert_equal false (T.Reader.TestUnnamedUnion.has_after field);
  in
  let () =
    let field = R.unnamed2_get reader in
    begin match T.Reader.TestUnnamedUnion.get field with
    | T.Reader.TestUnnamedUnion.Bar x when x = (Uint32.of_int 321) ->
        ()
    | _ -> assert_failure "bad unnamed2 default"
    end;
    assert_equal "foo" (T.Reader.TestUnnamedUnion.before_get field);
    assert_equal "bar" (T.Reader.TestUnnamedUnion.after_get field)
  in
  ()


module type TEST_LISTS = sig
  type t
  type array_t
  type 'a message_t
  type access
  type message_access
  type test_all_types_t

  module Struct8 : sig
    type t
    val f_get : t -> int
    val of_message : message_access message_t -> t
  end
  module Struct16c : sig
    type t
    val f_get : t -> int
    val has_pad : t -> bool
    val pad_get : t -> string
    val of_message : message_access message_t -> t
  end
  module Struct64 : sig
    type t
    val f_get : t -> Uint64.t
    val f_get_int_exn : t -> int
    val of_message : message_access message_t -> t
  end
  module Struct8c : sig
    type t
    val f_get : t -> int
    val has_pad : t -> bool
    val pad_get : t -> string
    val of_message : message_access message_t -> t
  end
  module StructP : sig
    type t
    val has_f : t -> bool
    val f_get : t -> string
    val of_message : message_access message_t -> t
  end
  module Struct32c : sig
    type t
    val f_get : t -> Uint32.t
    val f_get_int_exn : t -> int
    val has_pad : t -> bool
    val pad_get : t -> string
    val of_message : message_access message_t -> t
  end
  module Struct0c : sig
    type t
    val f_get : t -> unit
    val has_pad : t -> bool
    val pad_get : t -> string
    val of_message : message_access message_t -> t
  end
  module Struct32 : sig
    type t
    val f_get : t -> Uint32.t
    val f_get_int_exn : t -> int
    val of_message : message_access message_t -> t
  end
  module StructPc : sig
    type t
    val has_f : t -> bool
    val f_get : t -> string
    val pad_get : t -> Uint64.t
    val pad_get_int_exn : t -> int
    val of_message : message_access message_t -> t
  end
  module Struct0 : sig
    type t
    val f_get : t -> unit
    val of_message : message_access message_t -> t
  end
  module Struct64c : sig
    type t
    val f_get : t -> Uint64.t
    val f_get_int_exn : t -> int
    val has_pad : t -> bool
    val pad_get : t -> string
    val of_message : message_access message_t -> t
  end
  module Struct16 : sig
    type t
    val f_get : t -> int
    val of_message : message_access message_t -> t
  end
  module Struct1c : sig
    type t
    val f_get : t -> bool
    val has_pad : t -> bool
    val pad_get : t -> string
    val of_message : message_access message_t -> t
  end
  module Struct1 : sig
    type t
    val f_get : t -> bool
    val of_message : message_access message_t -> t
  end
  val has_list0 : t -> bool
  val list0_get : t -> (access, Struct0.t, array_t) Capnp.Array.t
  val list0_get_list : t -> Struct0.t list
  val list0_get_array : t -> Struct0.t array
  val has_list1 : t -> bool
  val list1_get : t -> (access, Struct1.t, array_t) Capnp.Array.t
  val list1_get_list : t -> Struct1.t list
  val list1_get_array : t -> Struct1.t array
  val has_list8 : t -> bool
  val list8_get : t -> (access, Struct8.t, array_t) Capnp.Array.t
  val list8_get_list : t -> Struct8.t list
  val list8_get_array : t -> Struct8.t array
  val has_list16 : t -> bool
  val list16_get : t -> (access, Struct16.t, array_t) Capnp.Array.t
  val list16_get_list : t -> Struct16.t list
  val list16_get_array : t -> Struct16.t array
  val has_list32 : t -> bool
  val list32_get : t -> (access, Struct32.t, array_t) Capnp.Array.t
  val list32_get_list : t -> Struct32.t list
  val list32_get_array : t -> Struct32.t array
  val has_list64 : t -> bool
  val list64_get : t -> (access, Struct64.t, array_t) Capnp.Array.t
  val list64_get_list : t -> Struct64.t list
  val list64_get_array : t -> Struct64.t array
  val has_list_p : t -> bool
  val list_p_get : t -> (access, StructP.t, array_t) Capnp.Array.t
  val list_p_get_list : t -> StructP.t list
  val list_p_get_array : t -> StructP.t array
  val has_int32_list_list : t -> bool
  val int32_list_list_get : t -> (access, (access, int32, array_t) Capnp.Array.t, array_t) Capnp.Array.t
  val int32_list_list_get_list : t -> (access, int32, array_t) Capnp.Array.t list
  val int32_list_list_get_array : t -> (access, int32, array_t) Capnp.Array.t array
  val has_text_list_list : t -> bool
  val text_list_list_get : t -> (access, (access, string, array_t) Capnp.Array.t, array_t) Capnp.Array.t
  val text_list_list_get_list : t -> (access, string, array_t) Capnp.Array.t list
  val text_list_list_get_array : t -> (access, string, array_t) Capnp.Array.t array
  val has_struct_list_list : t -> bool
  val struct_list_list_get : t -> (access, (access, test_all_types_t, array_t) Capnp.Array.t, array_t) Capnp.Array.t
  val struct_list_list_get_list : t -> (access, test_all_types_t, array_t) Capnp.Array.t list
  val struct_list_list_get_array : t -> (access, test_all_types_t, array_t) Capnp.Array.t array
  val of_message : message_access message_t -> t
end



let init_list_defaults (lists : T.Builder.TestLists.t) =
  let open T.Builder.TestLists in

  (* FIXME: skipping list0 and list1... at present we don't support encoding
     lists of single field structs of Void or Bool as List<Void>/List<Bool>. *)

  (*
  let list0  = list0_init lists 2 in
  let list1  = list1_init lists 4 in
  *)
  let list8  = list8_init lists 2 in
  let list16 = list16_init lists 2 in
  let list32 = list32_init lists 2 in
  let list64 = list64_init lists 2 in
  let listp  = list_p_init lists 2 in

  (*
  Struct0.f_set (Capnp.Array.get list0 0);
  Struct0.f_set (Capnp.Array.get list0 1);
  Struct1.f_set (Capnp.Array.get list1 0) true;
  Struct1.f_set (Capnp.Array.get list1 1) false;
  Struct1.f_set (Capnp.Array.get list1 2) true;
  Struct1.f_set (Capnp.Array.get list1 3) true;
  *)
  Struct8.f_set_exn (Capnp.Array.get list8 0) 123;
  Struct8.f_set_exn (Capnp.Array.get list8 1) 45;
  Struct16.f_set_exn (Capnp.Array.get list16 0) 12345;
  Struct16.f_set_exn (Capnp.Array.get list16 1) 6789;
  Struct32.f_set (Capnp.Array.get list32 0) (Uint32.of_int 123456789);
  Struct32.f_set (Capnp.Array.get list32 1) (Uint32.of_int 234567890);
  Struct64.f_set (Capnp.Array.get list64 0) (Uint64.of_string "1234567890123456");
  Struct64.f_set (Capnp.Array.get list64 1) (Uint64.of_string "2345678901234567");
  StructP.f_set (Capnp.Array.get listp 0) "foo";
  StructP.f_set (Capnp.Array.get listp 1) "bar";

  let () =
    let a = int32_list_list_init lists 3 in
    Capnp.Array.set_list (Capnp.Array.get a 0) [ 1l; 2l; 3l ];
    Capnp.Array.set_list (Capnp.Array.get a 1) [ 4l; 5l ];
    Capnp.Array.set_list (Capnp.Array.get a 2) [ 12341234l ]
  in
  let () =
    let a = text_list_list_init lists 3 in
    Capnp.Array.set_array (Capnp.Array.get a 0) [| "foo"; "bar" |];
    Capnp.Array.set_array (Capnp.Array.get a 1) [| "baz" |];
    Capnp.Array.set_array (Capnp.Array.get a 2) [| "qux"; "corge" |]
  in
  let () =
    let a = struct_list_list_init lists 2 in
    let () =
      let a0 = Capnp.Array.get a 0 in
      Capnp.Array.init a0 2;
      T.Builder.TestAllTypes.int32_field_set (Capnp.Array.get a0 0) 123l;
      T.Builder.TestAllTypes.int32_field_set (Capnp.Array.get a0 1) 456l
    in
    let () =
      let a1 = Capnp.Array.get a 1 in
      Capnp.Array.init a1 1;
      T.Builder.TestAllTypes.int32_field_set (Capnp.Array.get a1 0) 789l
    in
    ()
  in
  ()


module Check_test_list
    (TL : TEST_LISTS)
    (TAT : TEST_ALL_TYPES with type t = TL.test_all_types_t)
= struct
  let f (lists : TL.t) =

    (* FIXME: skipping list0 and list1... at present we don't support encoding
       lists of single field structs of Void or Bool as List<Void>/List<Bool>. *)

    (*
    assert_equal 2 (Capnp.Array.length (TL.list0_get lists));
    assert_equal 4 (Capnp.Array.length (TL.list1_get lists));
    *)
    assert_equal 2 (Capnp.Array.length (TL.list8_get lists));
    assert_equal 2 (Capnp.Array.length (TL.list16_get lists));
    assert_equal 2 (Capnp.Array.length (TL.list32_get lists));
    assert_equal 2 (Capnp.Array.length (TL.list64_get lists));
    assert_equal 2 (Capnp.Array.length (TL.list_p_get lists));

    (*
    assert_equal () (TL.Struct0.f_get (Capnp.Array.get (TL.list0_get lists) 0));
    assert_equal () (TL.Struct0.f_get (Capnp.Array.get (TL.list0_get lists) 1));
    assert_equal true  (TL.Struct1.f_get (Capnp.Array.get (TL.list1_get lists) 0));
    assert_equal false (TL.Struct1.f_get (Capnp.Array.get (TL.list1_get lists) 1));
    assert_equal true  (TL.Struct1.f_get (Capnp.Array.get (TL.list1_get lists) 2));
    assert_equal true  (TL.Struct1.f_get (Capnp.Array.get (TL.list1_get lists) 3));
    *)
    assert_equal 123 (TL.Struct8.f_get (Capnp.Array.get (TL.list8_get lists) 0));
    assert_equal 45  (TL.Struct8.f_get (Capnp.Array.get (TL.list8_get lists) 1));
    assert_equal 12345 (TL.Struct16.f_get (Capnp.Array.get (TL.list16_get lists) 0));
    assert_equal 6789  (TL.Struct16.f_get (Capnp.Array.get (TL.list16_get lists) 1));
    assert_equal (Uint32.of_int 123456789)
      (TL.Struct32.f_get (Capnp.Array.get (TL.list32_get lists) 0));
    assert_equal (Uint32.of_int 234567890)
      (TL.Struct32.f_get (Capnp.Array.get (TL.list32_get lists) 1));
    assert_equal (Uint64.of_string "1234567890123456")
      (TL.Struct64.f_get (Capnp.Array.get (TL.list64_get lists) 0));
    assert_equal (Uint64.of_string "2345678901234567")
      (TL.Struct64.f_get (Capnp.Array.get (TL.list64_get lists) 1));
    assert_equal "foo" (TL.StructP.f_get (Capnp.Array.get (TL.list_p_get lists) 0));
    assert_equal "bar" (TL.StructP.f_get (Capnp.Array.get (TL.list_p_get lists) 1));

    let () =
      let a = TL.int32_list_list_get lists in
      assert_equal 3 (Capnp.Array.length a);
      assert_equal (Capnp.Array.to_list (Capnp.Array.get a 0)) [ 1l; 2l; 3l ];
      assert_equal (Capnp.Array.to_list (Capnp.Array.get a 1)) [ 4l; 5l ];
      assert_equal (Capnp.Array.to_list (Capnp.Array.get a 2)) [ 12341234l ]
    in
    let () =
      let a = TL.text_list_list_get lists in
      assert_equal 3 (Capnp.Array.length a);
      assert_equal (Capnp.Array.to_list (Capnp.Array.get a 0)) [ "foo"; "bar" ];
      assert_equal (Capnp.Array.to_list (Capnp.Array.get a 1)) [ "baz" ];
      assert_equal (Capnp.Array.to_list (Capnp.Array.get a 2)) [ "qux"; "corge" ]
    in
    let () =
      let a = TL.struct_list_list_get lists in
      assert_equal 2 (Capnp.Array.length a);
      let e0 = Capnp.Array.get a 0 in
      assert_equal 2 (Capnp.Array.length e0);
      assert_equal 123l (TAT.int32_field_get (Capnp.Array.get e0 0));
      assert_equal 456l (TAT.int32_field_get (Capnp.Array.get e0 1));
      let e1 = Capnp.Array.get a 1 in
      assert_equal 1 (Capnp.Array.length e1);
      assert_equal 789l (TAT.int32_field_get (Capnp.Array.get e1 0))
    in
    ()
end


module ReaderTestLists = struct
  include T.Reader.TestLists
  type array_t = T.Reader.array_t
  type 'a message_t = 'a T.message_t
  type access = Test.ro
  type message_access = Test.ro
  type test_all_types_t = T.Reader.TestAllTypes.t
end

module BuilderTestLists = struct
  include T.Builder.TestLists
  type array_t = T.Builder.array_t
  type 'a message_t = 'a T.message_t
  type access = Test.rw
  type message_access = Test.rw
  type test_all_types_t = T.Builder.TestAllTypes.t
end

module Reader_check_test_list =
  Check_test_list(ReaderTestLists)(ReaderTestAllTypes)

module Builder_check_test_list =
  Check_test_list(BuilderTestLists)(BuilderTestAllTypes)


let test_list_defaults ctx =
  let root = T.Builder.TestListDefaults.init_root () in
  let lists = T.Builder.TestListDefaults.lists_get root in
  Reader_check_test_list.f (T.Reader.TestLists.of_builder lists);
  Builder_check_test_list.f lists;
  Reader_check_test_list.f (T.Reader.TestLists.of_builder lists)


let test_build_list_defaults ctx =
  let root = T.Builder.TestLists.init_root () in
  let () = init_list_defaults root in
  Reader_check_test_list.f (T.Reader.TestLists.of_builder root);
  Builder_check_test_list.f root;
  Reader_check_test_list.f (T.Reader.TestLists.of_builder root)


let test_upgrade_struct_in_builder ctx =
  let old_reader, message =
    let open T.Builder.TestOldVersion in
    let root = init_root () in
    old1_set root 123L;
    old2_set root "foo";
    let sub = old3_init root in
    old1_set sub 456L;
    old2_set sub "bar";
    (to_reader root, to_message root)
  in

  let () =
    let module B = T.Builder.TestNewVersion in
    let module R = T.Reader.TestOldVersion in
    let new_version = B.of_message message in

    (* The old instance should have been zero'd. *)
    assert_equal 0L (R.old1_get old_reader);
    assert_equal "" (R.old2_get old_reader);
    assert_equal 0L (R.old1_get (R.old3_get old_reader));
    assert_equal "" (R.old2_get (R.old3_get old_reader));

    assert_equal 123L (B.old1_get new_version);
    assert_equal "foo" (B.old2_get new_version);
    assert_equal 987L (B.new1_get new_version);
    assert_equal "baz" (B.new2_get new_version);

    let sub = B.old3_get new_version in
    assert_equal 456L (B.old1_get sub);
    assert_equal "bar" (B.old2_get sub);
    assert_equal 987L (B.new1_get sub);
    assert_equal "baz" (B.new2_get sub);

    B.old1_set new_version 234L;
    B.old2_set new_version "qux";
    B.new1_set new_version 321L;
    B.new2_set new_version "quux";

    B.old1_set sub 567L;
    B.old2_set sub "corge";
    B.new1_set sub 654L;
    B.new2_set sub "grault"
  in

  let () =
    (* Go back to the old version.  It should retain the values set on
       the new version. *)
    let open T.Builder.TestOldVersion in
    let old_version = of_message message in
    assert_equal 234L (old1_get old_version);
    assert_equal "qux" (old2_get old_version);

    let sub = old3_get old_version in
    assert_equal 567L (old1_get sub);
    assert_equal "corge" (old2_get sub);

    (* Overwrite the old fields.  The new fields should remain intact. *)
    old1_set old_version 345L;
    old2_set old_version "garply";
    old1_set sub 678L;
    old2_set sub "waldo"
  in

  let () =
    (* Back to the new version again. *)
    let open T.Reader.TestNewVersion in
    let new_version = of_message message in

    assert_equal 345L (old1_get new_version);
    assert_equal "garply" (old2_get new_version);
    assert_equal 321L (new1_get new_version);
    assert_equal "quux" (new2_get new_version);

    let sub = old3_get new_version in
    assert_equal 678L (old1_get sub);
    assert_equal "waldo" (old2_get sub);
    assert_equal 654L (new1_get sub);
    assert_equal "grault" (new2_get sub)
  in
  ()


let check_upgraded_list message expected_data expected_pointers =
  let () =
    let tnvl = TL.Builder.TestNewVersionList.of_message message in
    let builder_list = TL.Builder.TestNewVersionList.a_get tnvl in
    assert_equal (Capnp.Array.length builder_list) (Array.length expected_data);
    let module R = T.Reader.TestNewVersion in
    let module B = T.Builder.TestNewVersion in
    for i = 0 to Array.length expected_data - 1 do
      let builder = Capnp.Array.get builder_list i in
      assert_equal expected_data.(i) (B.old1_get builder);
      assert_equal expected_pointers.(i) (B.old2_get builder);

      (* Other fields shouldn't be set *)
      let reader = B.to_reader builder in
      assert_equal 0L (R.old1_get (R.old3_get reader));
      assert_equal "" (R.old2_get (R.old3_get reader));
      assert_equal 987L (R.new1_get reader);
      assert_equal "baz" (R.new2_get reader);

      (* Write some new data *)
      B.old1_set_int builder (i * 123);
      B.old2_set builder (Printf.sprintf "qux%d" i);
      B.new1_set_int builder (i * 456);
      B.new2_set builder (Printf.sprintf "corge%d" i);
    done
  in

  (* Read the newly-written data back as TestOldVersion to ensure
     it was updated. *)
  let () =
    let tovl = TL.Builder.TestOldVersionList.of_message message in
    let builder_list = TL.Builder.TestOldVersionList.a_get tovl in
    assert_equal (Capnp.Array.length builder_list) (Array.length expected_data);
    let module R = T.Reader.TestOldVersion in
    let module B = T.Builder.TestOldVersion in
    for i = 0 to Array.length expected_data - 1 do
      let builder = Capnp.Array.get builder_list i in
      assert_equal (B.old1_get_int_exn builder) (i * 123);
      assert_equal (B.old2_get builder) (Printf.sprintf "qux%d" i);
    done
  in

  (* Also read back as TestNewVersion again. *)
  let () =
    let tnvl = TL.Builder.TestNewVersionList.of_message message in
    let builder_list = TL.Builder.TestNewVersionList.a_get tnvl in
    assert_equal (Capnp.Array.length builder_list) (Array.length expected_data);
    let module R = T.Reader.TestNewVersion in
    let module B = T.Builder.TestNewVersion in
    for i = 0 to Array.length expected_data - 1 do
      let builder = Capnp.Array.get builder_list i in
      assert_equal (B.old1_get_int_exn builder) (i * 123);
      assert_equal (B.old2_get builder) (Printf.sprintf "qux%d" i);
      assert_equal (B.new1_get_int_exn builder) (i * 456);
      assert_equal (B.new2_get builder) (Printf.sprintf "corge%d" i);
    done
  in
  ()


let test_upgrade_list_in_builder ctx =
  let () =
    let root = TL.Builder.VoidList.init_root () in
    let (_ : (_, _, _) Capnp.Array.t) =
        TL.Builder.VoidList.a_set_list root [ (); (); (); (); ] in
    assert_equal (TL.Builder.VoidList.a_get_list root) [ (); (); (); (); ];
    check_upgraded_list (TL.Builder.VoidList.to_message root)
      [| 0L; 0L; 0L; 0L |] [| ""; ""; ""; "" |];
  in

  let () =
    let root = TL.Builder.UInt8List.init_root () in
    let a = TL.Builder.UInt8List.a_set_list root [ 0x12; 0x23; 0x33; 0x44 ] in
    assert_equal (TL.Builder.UInt8List.a_get_list root) [ 0x12; 0x23; 0x33; 0x44 ];
    check_upgraded_list (TL.Builder.UInt8List.to_message root)
      [| 0x12L; 0x23L; 0x33L; 0x44L |] [| ""; ""; ""; "" |];
    (* old location zero'd during upgrade *)
    assert_equal (Capnp.Array.to_list a) [ 0; 0; 0; 0 ];
  in

  let () =
    let root = TL.Builder.UInt16List.init_root () in
    let a = TL.Builder.UInt16List.a_set_list root
        [ 0x5612; 0x7823; 0xab33; 0xcd44 ]
    in
    assert_equal (TL.Builder.UInt16List.a_get_list root)
      [ 0x5612; 0x7823; 0xab33; 0xcd44 ];
    check_upgraded_list (TL.Builder.UInt16List.to_message root)
      [| 0x5612L; 0x7823L; 0xab33L; 0xcd44L |] [| ""; ""; ""; "" |];
    (* old location zero'd during upgrade *)
    assert_equal (Capnp.Array.to_list a) [ 0; 0; 0; 0 ];
  in

  let () =
    let u32_list = (List.map ~f:Uint32.of_int
        [ 0x17595612; 0x29347823; 0x5923ab32; 0x1a39cd45 ])
    in
    let root = TL.Builder.UInt32List.init_root () in
    let a = TL.Builder.UInt32List.a_set_list root u32_list in
    assert_equal (TL.Builder.UInt32List.a_get_list root) u32_list;
    check_upgraded_list (TL.Builder.UInt32List.to_message root)
      (Array.of_list
         (List.map ~f:(fun x -> x |> Uint32.to_int32 |> Int64.of_int32) u32_list))
      [| ""; ""; ""; "" |];
    (* old location zero'd during upgrade *)
    assert_equal (Capnp.Array.to_list a)
      [ Uint32.zero; Uint32.zero; Uint32.zero; Uint32.zero ]
  in

  let () =
    let u64_list = (List.map ~f:Uint64.of_int
        [0x1234abcd8735fe21; 0x7173bc0e1923af36])
    in
    let root = TL.Builder.UInt64List.init_root () in
    let a = TL.Builder.UInt64List.a_set_list root u64_list in
    assert_equal (TL.Builder.UInt64List.a_get_list root) u64_list;
    check_upgraded_list (TL.Builder.UInt64List.to_message root)
      (Array.of_list (List.map ~f:Uint64.to_int64 u64_list))
      [| ""; "" |];
    (* old location zero'd during upgrade *)
    assert_equal (Capnp.Array.to_list a) [ Uint64.zero; Uint64.zero; ]
  in

  let () =
    let root = TL.Builder.TextList.init_root () in
    let a = TL.Builder.TextList.a_set_list root [ "foo"; "bar"; "baz" ] in
    assert_equal (TL.Builder.TextList.a_get_list root) [ "foo"; "bar"; "baz" ];
    check_upgraded_list (TL.Builder.TextList.to_message root)
      [| 0L; 0L; 0L |] [| "foo"; "bar"; "baz" |];
    (* old location zero'd during upgrade *)
    assert_equal (Capnp.Array.to_list a) [ ""; ""; "" ]
  in

  let () =
    let (message, orig) =
      let root = TL.Builder.TestOldVersionList.init_root () in
      let a = TL.Builder.TestOldVersionList.a_init root 3 in
      let open T.Builder.TestOldVersion in
      old1_set (Capnp.Array.get a 0) 0x1234567890abcdefL;
      old1_set (Capnp.Array.get a 1) 0x234567890abcdef1L;
      old1_set (Capnp.Array.get a 2) 0x34567890abcdef12L;
      old2_set (Capnp.Array.get a 0) "foo";
      old2_set (Capnp.Array.get a 1) "bar";
      old2_set (Capnp.Array.get a 2) "baz";
      (TL.Builder.TestOldVersionList.to_message root, a)
    in
    let () =
      let root = TL.Reader.VoidList.of_message message in
      assert_equal (TL.Reader.VoidList.a_get_list root) [ (); (); () ];
    in
    let () =
      let root = TL.Reader.BoolList.of_message message in
      assert_equal (TL.Reader.BoolList.a_get_list root) [ true; true; false ];
    in
    let () =
      let root = TL.Reader.UInt8List.of_message message in
      assert_equal (TL.Reader.UInt8List.a_get_list root) [ 0xef; 0xf1; 0x12 ];
    in
    let () =
      let root = TL.Reader.UInt16List.of_message message in
      assert_equal (TL.Reader.UInt16List.a_get_list root) [ 0xcdef; 0xdef1; 0xef12 ];
    in
    let () =
      let root = TL.Reader.UInt32List.of_message message in
      assert_equal (TL.Reader.UInt32List.a_get_list root)
        (List.map ~f:Uint32.of_string [ "0x90abcdef"; "0x0abcdef1"; "0xabcdef12" ])
    in
    let () =
      let root = TL.Reader.UInt64List.of_message message in
      assert_equal (TL.Reader.UInt64List.a_get_list root)
        (List.map ~f:Uint64.of_string
           ["0x1234567890abcdef"; "0x234567890abcdef1"; "0x34567890abcdef12" ])
    in
    let () =
      let root = TL.Reader.TextList.of_message message in
      assert_equal (TL.Reader.TextList.a_get_list root) [ "foo"; "bar"; "baz" ]
    in
    check_upgraded_list message
      [| 0x1234567890abcdefL; 0x234567890abcdef1L; 0x34567890abcdef12L |]
      [| "foo"; "bar"; "baz" |];
    (* old location zero'd during upgrade *)
    let module R = T.Reader.TestOldVersion in
    assert_equal (List.map ~f:(fun x -> x |> R.of_builder |> R.old1_get)
        (Capnp.Array.to_list orig)) [ 0L; 0L; 0L ];
    assert_equal (List.map ~f:(fun x -> x |> R.of_builder |> R.old2_get)
        (Capnp.Array.to_list orig)) [ ""; ""; "" ]
  in
  ()


let test_nested_types_encoding ctx =
  let open T.Reader.TestNestedTypes in
  let reader = of_builder (T.Builder.TestNestedTypes.init_root ()) in
  assert_equal NestedEnum.Bar (outer_nested_enum_get reader);
  assert_equal NestedStruct.NestedEnum.Quux (inner_nested_enum_get reader);

  let nested = nested_struct_get reader in
  assert_equal NestedEnum.Bar
    (NestedStruct.outer_nested_enum_get nested);
  assert_equal NestedStruct.NestedEnum.Quux
    (NestedStruct.inner_nested_enum_get nested)


let test_imports ctx =
  let () =
    let root = TI.Builder.TestImport.init_root () in
    let () = init_test_message (TI.Builder.TestImport.field_init root) in
    let reader = TI.Builder.TestImport.to_reader root in
    Reader_check_test_message.f (TI.Reader.TestImport.field_get reader)
  in
  ()

let test_constants ctx =
  let open T.Reader.TestConstants in
  assert_equal () void_const;
  assert_equal true bool_const;
  assert_equal (-123) int8_const;
  assert_equal (-12345) int16_const;
  assert_equal (-12345678l) int32_const;
  assert_equal (-123456789012345L) int64_const;
  assert_equal 234 uint8_const;
  assert_equal 45678 uint16_const;
  assert_equal (Uint32.of_int 3456789012) uint32_const;
  assert_equal (Uint64.of_string "12345678901234567890") uint64_const;
  assert_float32_equal 1234.5 float32_const;
  assert_float64_equal (-123e45) float64_const;
  assert_equal "foo" text_const;
  assert_equal "bar" data_const;
  let () =
    let open T.Reader.TestAllTypes in
    assert_equal () (void_field_get struct_const);
    assert_equal true (bool_field_get struct_const);
    assert_equal (-12) (int8_field_get struct_const);
    assert_equal 3456 (int16_field_get struct_const);
    assert_equal (-78901234l) (int32_field_get struct_const);
    assert_equal 56789012345678L (int64_field_get struct_const);
    assert_equal 90 (u_int8_field_get struct_const);
    assert_equal 1234 (u_int16_field_get struct_const);
    assert_equal (Uint32.of_int 56789012) (u_int32_field_get struct_const);
    assert_equal (Uint64.of_string "345678901234567890") (u_int64_field_get struct_const);
    assert_float32_equal (-1.25e-10) (float32_field_get struct_const);
    assert_float64_equal 345.0 (float64_field_get struct_const);
    assert_equal "baz" (text_field_get struct_const);
    assert_equal "qux" (data_field_get struct_const);
    assert_equal "nested" (text_field_get (struct_field_get struct_const));
    assert_equal "really nested"
      (struct_const |> struct_field_get |> struct_field_get |> text_field_get);
    assert_equal T.Reader.TestEnum.Baz (enum_field_get struct_const);

    assert_equal (void_list_get_list struct_const) [ (); (); () ];
    assert_equal (bool_list_get_list struct_const) [ false; true; false; true; true ];
    assert_equal (int8_list_get_list struct_const) [ 12; -34; -0x80; 0x7f ];
    assert_equal (int16_list_get_list struct_const) [ 1234; -5678; -0x8000; 0x7fff ];
    assert_equal (int32_list_get_list struct_const)
      [ 12345678l; -90123456l; -0x80000000l; 0x7fffffffl ];
    assert_equal (int64_list_get_list struct_const)
      [ 123456789012345L; -678901234567890L; -0x8000000000000000L; 0x7fffffffffffffffL ];
    assert_equal (u_int8_list_get_list struct_const) [ 12; 34; 0; 0xff ];
    assert_equal (u_int16_list_get_list struct_const) [ 1234; 5678; 0; 0xffff ];
    assert_equal (u_int32_list_get_list struct_const) (List.map ~f:Uint32.of_string
        [ "12345678"; "90123456"; "0"; "0xffffffff" ]);
    assert_equal (u_int64_list_get_list struct_const) (List.map ~f:Uint64.of_string
        [ "123456789012345"; "678901234567890"; "0"; "0xffffffffffffffff" ]);
    List.iter2_exn (float32_list_get_list struct_const) 
      [ 0.0; 1234567.0; 1e37; -1e37; 1e-37; -1e-37 ] ~f:assert_float32_equal;
    List.iter2_exn (float64_list_get_list struct_const)
      [ 0.0; 123456789012345.0; 1e306; -1e306; 1e-306; -1e-306 ] ~f:assert_float64_equal;
    assert_equal (text_list_get_list struct_const) [ "quux"; "corge"; "grault" ];
    assert_equal (data_list_get_list struct_const) [ "garply"; "waldo"; "fred" ];
    assert_equal 3 (Capnp.Array.length (struct_list_get struct_const));
    assert_equal "x structlist 1"
      (text_field_get (Capnp.Array.get (struct_list_get struct_const) 0));
    assert_equal "x structlist 2"
      (text_field_get (Capnp.Array.get (struct_list_get struct_const) 1));
    assert_equal "x structlist 3"
      (text_field_get (Capnp.Array.get (struct_list_get struct_const) 2));
    assert_equal (enum_list_get_list struct_const)
      [ T.Reader.TestEnum.Qux; T.Reader.TestEnum.Bar; T.Reader.TestEnum.Grault ]
  in
  assert_equal T.Reader.TestEnum.Corge enum_const;

  assert_equal 6 (Capnp.Array.length void_list_const);
  assert_equal (Capnp.Array.to_list bool_list_const) [ true; false; false; true ];
  assert_equal (Capnp.Array.to_list int8_list_const) [ 111; -111 ];
  assert_equal (Capnp.Array.to_list int16_list_const) [ 11111; -11111 ];
  assert_equal (Capnp.Array.to_list int32_list_const)
    [ 111111111l; -111111111l ];
  assert_equal (Capnp.Array.to_list int64_list_const)
    [ 1111111111111111111L; -1111111111111111111L ];
  assert_equal (Capnp.Array.to_list uint8_list_const) [ 111; 222 ];
  assert_equal (Capnp.Array.to_list uint16_list_const) [ 33333; 44444 ];
  assert_equal (Capnp.Array.to_list uint32_list_const)
    [ Uint32.of_string "3333333333" ];
  assert_equal (Capnp.Array.to_list uint64_list_const)
    [ Uint64.of_string "11111111111111111111" ];

  assert_equal 4 (Capnp.Array.length float32_list_const);
  assert_float32_equal 5555.5 (Capnp.Array.get float32_list_const 0);
  assert_equal Float.infinity (Capnp.Array.get float32_list_const 1);
  assert_equal Float.neg_infinity (Capnp.Array.get float32_list_const 2);
  assert_bool "nan"
    ((Capnp.Array.get float32_list_const 3) <> (Capnp.Array.get float32_list_const 3));

  assert_equal 4 (Capnp.Array.length float64_list_const);
  assert_float64_equal 7777.75 (Capnp.Array.get float64_list_const 0);
  assert_equal Float.infinity (Capnp.Array.get float64_list_const 1);
  assert_equal Float.neg_infinity (Capnp.Array.get float64_list_const 2);
  assert_bool "nan"
    ((Capnp.Array.get float64_list_const 3) <> (Capnp.Array.get float64_list_const 3));

  assert_equal (Capnp.Array.to_list text_list_const)
    [ "plugh"; "xyzzy"; "thud" ];
  assert_equal (Capnp.Array.to_list data_list_const)
    [ "oops"; "exhausted"; "rfc3092" ];

  let () =
    let open T.Reader.TestAllTypes in
    assert_equal 3 (Capnp.Array.length struct_list_const);
    assert_equal "structlist 1" (text_field_get (Capnp.Array.get struct_list_const 0));
    assert_equal "structlist 2" (text_field_get (Capnp.Array.get struct_list_const 1));
    assert_equal "structlist 3" (text_field_get (Capnp.Array.get struct_list_const 2))
  in
  assert_equal (Capnp.Array.to_list enum_list_const)
    [ T.Reader.TestEnum.Foo; T.Reader.TestEnum.Garply ]


let test_global_constants ctx =
  let open T.Reader.TestAllTypes in
  assert_equal (Uint32.of_int 12345) T.Reader.global_int;
  assert_equal "foobar" T.Reader.global_text;
  assert_equal 54321l (int32_field_get T.Reader.global_struct);
  assert_equal (Uint32.of_int 12345)
    (u_int32_field_get T.Reader.derived_constant);
  assert_equal "foo"
    (text_field_get T.Reader.derived_constant);
  let sub = struct_field_get T.Reader.derived_constant in
  assert_equal (text_list_get_list sub)
    [ "quux"; "corge"; "grault" ];
  assert_equal (int16_list_get_list T.Reader.derived_constant)
    [ 11111; -11111 ];
  let list_reader = struct_list_get T.Reader.derived_constant in
  assert_equal 3 (Capnp.Array.length list_reader);
  assert_equal "structlist 1" (text_field_get (Capnp.Array.get list_reader 0));
  assert_equal "structlist 2" (text_field_get (Capnp.Array.get list_reader 1));
  assert_equal "structlist 3" (text_field_get (Capnp.Array.get list_reader 2))


let test_int_accessors ctx =
  let module R = T.Reader.TestAllTypes in
  let module B = T.Builder.TestAllTypes in
  let root  = B.init_root () in
  let rroot = R.of_builder root in

  B.int64_field_set_int root Int.max_value;
  assert_equal (B.int64_field_get_int_exn root) Int.max_value;
  assert_equal (R.int64_field_get_int_exn rroot) Int.max_value;
  B.int64_field_set root (Int64.(+) (Int64.of_int Int.max_value) Int64.one);
  assert_raises_out_of_int_range (fun () -> B.int64_field_get_int_exn root);
  assert_raises_out_of_int_range (fun () -> R.int64_field_get_int_exn rroot);

  B.int64_field_set_int root Int.min_value;
  assert_equal (B.int64_field_get_int_exn root) Int.min_value;
  assert_equal (R.int64_field_get_int_exn rroot) Int.min_value;
  B.int64_field_set root (Int64.(-) (Int64.of_int Int.min_value) Int64.one);
  assert_raises_out_of_int_range (fun () -> B.int64_field_get_int_exn root);
  assert_raises_out_of_int_range (fun () -> R.int64_field_get_int_exn rroot);

  B.u_int64_field_set_int_exn root Int.max_value;
  assert_equal (B.u_int64_field_get_int_exn root) Int.max_value;
  assert_equal (R.u_int64_field_get_int_exn rroot) Int.max_value;
  B.u_int64_field_set root (Uint64.add (Uint64.of_int Int.max_value) Uint64.one);
  assert_raises_out_of_int_range (fun () -> B.u_int64_field_get_int_exn root);
  assert_raises_out_of_int_range (fun () -> R.u_int64_field_get_int_exn rroot);

  (* Assuming tests are running on 64-bit... *)

  B.int32_field_set_int_exn root (-0x80000000);
  B.int32_field_set_int_exn root 0x7fffffff;
  assert_raises_invalid_arg (fun () -> B.int32_field_set_int_exn root (-0x80000001));
  assert_raises_invalid_arg (fun () -> B.int32_field_set_int_exn root 0x80000000);

  B.u_int32_field_set_int_exn root 0;
  B.u_int32_field_set_int_exn root 0xffffffff;
  assert_raises_invalid_arg (fun () -> B.u_int32_field_set_int_exn root 0x100000000);
  assert_raises_invalid_arg (fun () -> B.u_int32_field_set_int_exn root (-1));

  B.u_int64_field_set_int_exn root 0;
  B.u_int64_field_set_int_exn root Int.max_value;
  assert_raises_invalid_arg (fun () -> B.u_int64_field_set_int_exn root (-1))



let encoding_suite =
  "all_types" >::: [
    "encode/decode" >:: test_encode_decode;
    "decode defaults" >:: test_decode_defaults;
    "init defaults" >:: test_init_defaults;
    "union encode/decode" >:: test_union_encoding;
    "union layout" >:: test_union_layout;
    "unnamed union encode/decode" >:: test_unnamed_union_encoding;
    "group encode/decode" >:: test_groups;
    "interleaved groups" >:: test_interleaved_groups;
    "union defaults" >:: test_union_defaults;
    "list defaults" >:: test_list_defaults;
    "build list defaults" >:: test_build_list_defaults;
    "upgrade struct in builder" >:: test_upgrade_struct_in_builder;
    "upgrade list in builder" >:: test_upgrade_list_in_builder;
    "nested types encoding" >:: test_nested_types_encoding;
    "test imports" >:: test_imports;
    "test constants" >:: test_constants;
    "test global constants" >:: test_global_constants;
    "test int accessors" >:: test_int_accessors;
  ]

let () = run_test_tt_main encoding_suite

