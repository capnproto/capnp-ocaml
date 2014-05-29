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
      and type inner_struct_t = Outer.inner_struct_t) = struct

  let f (s : Outer.t) : unit =
    let open Outer in
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
      let open Inner in
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
      let open Inner in
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
  let message =
    let builder = T.Builder.TestAllTypes.init_root () in
    let () = init_test_message builder in
    let () = Builder_check_test_message.f builder in
    T.Builder.TestAllTypes.to_message builder
  in
  let reader = T.Reader.TestAllTypes.of_message message in
  Reader_check_test_message.f reader


let test_decode_defaults ctx =
  let null_root = "\x00\x00\x00\x00\x00\x00\x00\x00" in
  let message = SM.Message.readonly (SM.Message.of_storage [ null_root ]) in
  let reader = T.Reader.TestDefaults.of_message message in
  Reader_check_test_defaults.f reader


let test_init_defaults ctx =
  let null_root = "\x00\x00\x00\x00\x00\x00\x00\x00" in
  let message = SM.Message.of_storage [ null_root ] in
  let () =
    let builder = T.Builder.TestDefaults.of_message message in
    (* First pass initializes [message] with defaults *)
    let () = Builder_check_test_defaults.f builder in
    (* Second pass just reads the initialized structure *)
    let () = Builder_check_test_defaults.f builder in
    ()
  in
  let reader = T.Reader.TestDefaults.of_message message in
  Reader_check_test_defaults.f reader


let encoding_suite =
  "all_types" >::: [
    "encode/decode" >:: test_encode_decode;
    "decode defaults" >:: test_decode_defaults;
    "init defaults" >:: test_init_defaults;
  ]

let () = run_test_tt_main encoding_suite

