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


module Array = struct
  type ('a, 'arr) storage_t = {
    storage : 'arr;
    length  : 'arr -> int;
    get     : 'arr -> int -> 'a;
  }

  (** The storage type is optional because an RArray may be represented as
      a null pointer within a read-only message.  The None case is simply
      treated as an empty array. *)
  type ('a, 'arr) t = ('a, 'arr) storage_t option

  let length x =
    match x with
    | Some storage ->
        storage.length storage.storage
    | None ->
        0

  let get x i =
    match x with
    | Some storage ->
        storage.get storage.storage i
    | None ->
        invalid_arg "index out of bounds"

  let make ~length ~get storage = Some { storage; length; get }

  let make_default () = None
end


module BArray = struct
  type ('a, 'arr) storage_t = {
    storage : 'arr;
    length  : 'arr -> int;
    get     : 'arr -> int -> 'a;
    set     : 'arr -> int -> 'a -> unit;
  }

  (** A Builder will never use a null pointer for the backing storage of
      a BArray--array storage is always allocated within the message as soon
      as the array pointer is dereferenced.  Therefore the storage_t is not
      optional. *)
  type ('a, 'arr) t = ('a, 'arr) storage_t

  let length x = x.length x.storage

  let get x i = x.get x.storage i

  let set x i v = x.set x.storage i v

  let make ~length ~get ~set storage = { storage; length; get; set }
end
