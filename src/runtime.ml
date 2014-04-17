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

module type ARRAY = sig
  type ('cap, 'a, 'arr) t

  val length : ('cap, 'a, 'arr) t -> int

  val get : ('cap, 'a, 'arr) t -> int -> 'a

  val set : (rw, 'a, 'arr) t -> int -> 'a -> unit
end

module InnerArray = struct
  type ('cap, 'a, 'arr) t = {
    length : unit -> int;
    get_unsafe : int -> 'a;
    set_unsafe : int -> 'a -> unit;
    storage : 'arr option;
  }

  let length x = x.length ()

  let get x i =
    if i < 0 || i >= x.length () then
      invalid_arg "index out of bounds"
    else
      x.get_unsafe i

  let set x i v =
    if i < 0 || i >= x.length () then
      invalid_arg "index out of bounds"
    else
      x.set_unsafe i v

  let of_outer_array x = x

  let to_outer_array x = x

  let to_storage x = x.storage

  let invalid_set_unsafe i v = assert false
end


module Array = struct
  type ('cap, 'a, 'arr) t = ('cap, 'a, 'arr) InnerArray.t

  let length = InnerArray.length
  let get    = InnerArray.get
  let set    = InnerArray.set
end


