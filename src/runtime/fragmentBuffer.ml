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
  mutable ends : (fragment * fragment) option;     (* Front, back *)
  (** String fragments stored in FIFO order *)

  mutable fragments_size : int;
  (** Total byte count of the fragments *)
}

let empty () = {
  ends = None;
  fragments_size = 0;
}

let add_fragment stream data =
  let len = String.length data in
  if len > 0 then (
    let fragment = { data; next = None } in
    begin match stream.ends with
      | Some (old_front, old_back) ->
        old_back.next <- Some fragment;
        stream.ends <- Some (old_front, fragment)
      | None ->
        stream.ends <- Some (fragment, fragment)
    end;
    stream.fragments_size <- stream.fragments_size + len
  )

let enqueue_front stream data =
  match stream.ends with
  | None ->
    let fragment = { data; next = None } in
    stream.ends <- Some (fragment, fragment)
  | Some (old_front, old_back) ->
    let fragment = { data; next = Some old_front } in
    stream.ends <- Some (fragment, old_back)

(* Note: does not update [fragments_size] *)
let pop stream =
  match stream.ends with
  | None ->
    assert (stream.fragments_size = 0);
    assert false
  | Some (fragment, old_back) ->
    begin match fragment.next with
    | None -> stream.ends <- None
    | Some new_front -> stream.ends <- Some (new_front, old_back)
    end;
    fragment.data

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
    Some (Bytes.unsafe_to_string buf)

let remove_at_least stream size =
  if stream.fragments_size < size then
    None
  else begin
    let buffer = Buffer.create size in
    while Buffer.length buffer < size do
      Buffer.add_string buffer (pop stream)
    done;
    stream.fragments_size <- stream.fragments_size - (Buffer.length buffer);
    Some (Buffer.contents buffer)
  end

let unremove stream data =
  let len = String.length data in
  if len > 0 then (
    enqueue_front stream data;
    stream.fragments_size <- stream.fragments_size + len;
  )

let peek_exact stream size =
  match remove_exact stream size with
  | Some bytes ->
    unremove stream bytes;
    Some bytes
  | None -> None
