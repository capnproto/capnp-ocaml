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


type fragment = {
  data : string;
  mutable next : fragment option;
}

type t = {
  id : int;
  mutable ends : fragment option;     (* Next data to consume *)
  mutable back : fragment option;      (* New items go here *)

  (** Total byte count of the fragments *)
  mutable fragments_size : int;
}

let id = ref 0

let empty () =
  incr id;
  {
    id = !id;
    front = None;
    back = None;
    fragments_size = 0;
  }

let pp_list f fs =
  let rec aux = function
    | None -> ()
    | Some x ->
      Format.fprintf f "%d " (String.length x.data);
      aux x.next
  in
  aux fs

let pp_state f s =
  Format.fprintf f "%d: len=%d [%a]" s.id s.fragments_size pp_list s.front

let add_fragment stream data =
  let len = String.length data in
  Format.printf "add_fragment(%d): %a@.%!" len pp_state stream;
  if len > 0 then (
    let fragment = { data; next = None } in
    begin match stream.back with
      | Some old_back -> old_back.next <- Some fragment
      | None -> stream.front <- Some fragment
    end;
    stream.back <- Some fragment;
    stream.fragments_size <- stream.fragments_size + len
  )

let enqueue_front stream data =
  let fragment = { data; next = stream.front } in
  if stream.front = None then stream.back <- Some fragment;
  stream.front <- Some fragment

(* Note: does not update [fragments_size] *)
let pop stream =
  Format.printf "pop: %a@.%!" pp_state stream;
  match stream.front with
  | None ->
    assert (stream.fragments_size = 0);
    assert false
  | Some fragment ->
    let data = fragment.data in
    stream.front <- fragment.next;
    data

let of_string s =
  let stream = empty () in
  let () = add_fragment stream s in
  stream

let byte_count stream = stream.fragments_size

let remove_exact stream size =
  Format.printf "remove_exact(%d): %a@.%!" size pp_state stream;
  if stream.fragments_size < size then
    None
  else
    let buf = Bytes.create size in
    let () =
      let ofs = ref 0 in
      while !ofs < size do
        let bytes_remaining = size - !ofs in
        let fragment = pop stream in
        let bytes_from_fragment = min bytes_remaining (String.length fragment) in
        Bytes.blit
          (Bytes.unsafe_of_string fragment) 0
          buf !ofs
          bytes_from_fragment;
        begin if bytes_from_fragment < String.length fragment then
          let remainder = Util.str_slice ~start:bytes_from_fragment fragment in
          enqueue_front stream remainder
        end;
        ofs := !ofs + bytes_from_fragment;
      done;
      stream.fragments_size <- stream.fragments_size - size;
    in
    assert ((stream.front = None) = (stream.fragments_size = 0));
    Some (Bytes.unsafe_to_string buf)

let remove_at_least stream size =
  Format.printf "remove_at_least(%d): %a@.%!" size pp_state stream;
  if stream.fragments_size < size then
    None
  else begin
    let buffer = Buffer.create size in
    while Buffer.length buffer < size do
      Buffer.add_string buffer (pop stream)
    done;
    stream.fragments_size <- stream.fragments_size - (Buffer.length buffer);
    assert ((stream.front = None) = (stream.fragments_size = 0));
    Some (Buffer.contents buffer)
  end

let unremove stream data =
  let len = String.length data in
  Format.printf "unremove(%d): %a@.%!" len pp_state stream;
  if len > 0 then (
    enqueue_front stream data;
    stream.fragments_size <- stream.fragments_size + len;
    assert ((stream.front = None) = (stream.fragments_size = 0));
  )

let peek_exact stream size =
  Format.printf "peek_exact(%d): %a@.%!" size pp_state stream;
  match remove_exact stream size with
  | None -> None
  | Some bytes ->
    unremove stream bytes;
    Some bytes
