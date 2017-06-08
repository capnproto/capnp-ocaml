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

(* Workaround for missing Caml.Bytes in Core 112.35.00 *)
module CamlBytes = Bytes

open Core_kernel.Std
module Bytes = CamlBytes


type t = {
  (** String fragments stored in FIFO order *)
  fragments : string Deque.t;

  (** Total byte count of the fragments *)
  mutable fragments_size : int;
}

let empty () = {
  fragments = Deque.create ();
  fragments_size = 0;
}

let add_fragment stream fragment =
  let len = String.length fragment in
  if len = 0 then
    ()
  else
    let () = Deque.enqueue_back stream.fragments fragment in
    stream.fragments_size <- stream.fragments_size + len

let of_string s =
  let stream = empty () in
  let () = add_fragment stream s in
  stream

let byte_count stream = stream.fragments_size

let remove_exact stream size =
  if stream.fragments_size < size then
    None
  else
    let buf = Bytes.create size in
    let () =
      let ofs = ref 0 in
      while !ofs < size do
        let bytes_remaining = size - !ofs in
        let fragment = Deque.dequeue_front_exn stream.fragments in
        let bytes_from_fragment = min bytes_remaining (String.length fragment) in
        Bytes.blit
          (Bytes.unsafe_of_string fragment) 0
          buf !ofs
          bytes_from_fragment;
        begin if bytes_from_fragment < String.length fragment then
          let remainder = Util.str_slice ~start:bytes_from_fragment fragment in
          Deque.enqueue_front stream.fragments remainder
        end;
        ofs := !ofs + bytes_from_fragment;
      done;
      stream.fragments_size <- stream.fragments_size - size;
    in
    Some (Bytes.unsafe_to_string buf)

let remove_at_least stream size =
  if stream.fragments_size < size then
    None
  else begin
    let buffer = Buffer.create size in
    while Buffer.length buffer < size do
      Buffer.add_string buffer (Deque.dequeue_front_exn stream.fragments)
    done;
    stream.fragments_size <- stream.fragments_size - (Buffer.length buffer);
    Some (Buffer.contents buffer)
  end

let peek_exact stream size =
  match remove_exact stream size with
  | Some bytes ->
      let () = Deque.enqueue_front stream.fragments bytes in
      let () = stream.fragments_size <- stream.fragments_size + size in
      Some bytes
  | None ->
      None

let unremove stream bytes =
  let len = String.length bytes in
  if len = 0 then
    ()
  else
    let () = Deque.enqueue_front stream.fragments bytes in
    stream.fragments_size <- stream.fragments_size + len


