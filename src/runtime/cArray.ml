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

open Core_kernel.Std

type ro = Message.ro
type rw = Message.rw

type ('cap, 'a, 'arr) t = ('cap, 'a, 'arr) InnerArray.t

let length = InnerArray.length
let get    = InnerArray.get
let set    = InnerArray.set 
let init   = InnerArray.init

let foldi a ~init ~f =
  let len = length a in
  let () = assert (len >= 0) in
  let rec loop acc i =
    if i = len then
      acc
    else
      let x = get a i in
      loop (f i acc x) (i + 1)
  in
  loop init 0

let foldi_right a ~f ~init =
  let rec loop acc i =
    if i < 0 then
      acc
    else
      let x = get a i in
      loop (f i x acc) (i - 1)
  in
  let len = length a in
  loop init (len - 1)

let fold a ~init ~f =
  foldi a ~init ~f:(fun _i acc x -> f acc x)

let fold_right a ~f ~init =
  foldi_right a ~f:(fun _i x acc -> f x acc) ~init

let mem ?equal a x =
  let len = length a in
  let () = assert (len >= 0) in
  let real_equal =
    match equal with
    | Some user_equal ->
        user_equal
    | None ->
        Pervasives.(=)
  in
  let rec loop i =
    if i = len then
      false
    else
      if real_equal x (get a i) then
        true
      else
        loop (i + 1)
  in
  loop 0

let is_empty a = (length a) = 0

let iter a ~f =
  fold a ~init:() ~f:(fun () x -> f x)

let iteri a ~f =
  foldi a ~init:() ~f:(fun i () x -> f i x)

let exists a ~f =
  let len = length a in
  let () = assert (len >= 0) in
  let rec loop i =
    if i = len then
      false
    else
      if f (get a i) then
        true
      else
        loop (i + 1)
  in
  loop 0

let forall a ~f =
  let len = length a in
  let () = assert (len >= 0) in
  let rec loop i =
    if i = len then
      true
    else
      if f (get a i) then
        loop (i + 1)
      else
        false
  in
  loop 0

let count a ~f =
  let fold_f acc x = if f x then acc + 1 else acc in
  fold a ~init:0 ~f:fold_f

let find a ~f =
  let len = length a in
  let () = assert (len >= 0) in
  let rec loop i =
    if i = len then
      None
    else
      let x = get a i in
      if f x then
        Some x
      else
        loop (i + 1)
  in
  loop 0

let find_map a ~f =
  let len = length a in
  let () = assert (len >= 0) in
  let rec loop i =
    if i = len then
      None
    else
      let r = f (get a i) in
      match r with
      | Some _ -> r
      | None   -> loop (i + 1)
  in
  loop 0

let to_list a =
  List.init (length a) ~f:(fun i -> get a i)

let to_array a =
  Core_kernel.Std.Array.init (length a) ~f:(fun i -> get a i)

let set_list a lst =
  let () = init a (List.length lst) in
  List.iteri lst ~f:(fun i x -> set a i x)

let set_array a arr =
  let () = init a (Array.length arr) in
  Array.iteri arr ~f:(fun i x -> set a i x)

let map_array a ~f =
  Core_kernel.Std.Array.init (length a) ~f:(fun i -> f (get a i))

let map_list a ~f =
  List.init (length a) ~f:(fun i -> f (get a i))


