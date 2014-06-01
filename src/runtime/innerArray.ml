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

type ro = Message.ro
type rw = Message.rw

type ('cap, 'a, 'arr) t = {
  mutable length : int;
  mutable storage : 'arr option;
  get_unsafe : 'arr -> int -> 'a;
  set_unsafe : 'arr -> int -> 'a -> unit;
  init : int -> 'arr;
}

let length x = x.length

let get x i =
  if i < 0 || i >= x.length then
    invalid_arg "index out of bounds"
  else
    match x.storage with
    | Some storage ->
        x.get_unsafe storage i
    | None ->
        assert false

let set x i v =
  if i < 0 || i >= x.length then
    invalid_arg "index out of bounds"
  else
    match x.storage with
    | Some storage ->
        x.set_unsafe storage i v
    | None ->
        assert false

let init x n =
  if n < 0 then
    invalid_arg "InnerArray.init"
  else
    let () = x.storage <- Some (x.init n) in
    x.length <- n

let to_storage x = x.storage

let invalid_get_unsafe a i = assert false

let invalid_set_unsafe a i v = assert false

let invalid_init n = assert false

