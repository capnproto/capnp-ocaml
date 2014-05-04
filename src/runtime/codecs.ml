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


open Core.Std


module FramingError = struct
  type t =
    | Incomplete    (* less than a full frame is available *)
    | Unsupported   (* frame describes a segment count or segment size that is too large *)
end


module FramedStream = struct

  type incomplete_frame_t = {
    frame_header      : string;
    complete_segments : string Res.Array.t;
  }

  type decoder_state_t =
    | IncompleteHeader
    | IncompleteFrame of incomplete_frame_t

  type t = {
    (** Primary storage for incoming stream segments. *)
    fragments : string Dequeue.t;

    (** Total byte count of all fragments. *)
    mutable fragments_size : int;

    (** Partially-decoded frame information *)
    mutable decoder_state : decoder_state_t;
  }

  let add_fragment stream fragment =
    if String.is_empty fragment then
      ()
    else
      let () = Dequeue.enqueue_back stream.fragments fragment in
      stream.fragments_size <- stream.fragments_size + String.length fragment

  let remove_bytes stream size =
    if stream.fragments_size < size then
      None
    else
      let buf = String.create size in
      let () =
        let ofs = ref 0 in
        while !ofs < size do
          let bytes_remaining = size - !ofs in
          let fragment = Dequeue.dequeue_front_exn stream.fragments in
          let bytes_from_fragment = min bytes_remaining (String.length fragment) in
          String.blit
            ~src:fragment ~src_pos:0
            ~dst:buf ~dst_pos:!ofs
            ~len:bytes_from_fragment;
          begin if bytes_from_fragment < String.length fragment then
            let remainder = String.slice fragment bytes_from_fragment 0 in
            Dequeue.enqueue_front stream.fragments remainder
          end;
          ofs := !ofs + bytes_from_fragment;
        done;
        stream.fragments_size <- stream.fragments_size - size;
      in
      Some buf

  let peek_bytes stream size =
    match remove_bytes stream size with
    | Some bytes ->
        let () = Dequeue.enqueue_front stream.fragments bytes in
        let () = stream.fragments_size <- stream.fragments_size + size in
        Some bytes
    | None ->
        None


  let empty () = {
    fragments = Dequeue.create ();
    fragments_size = 0;
    decoder_state = IncompleteHeader
  }

  let of_string s =
    let stream = empty () in
    let () = add_fragment stream s in
    stream

  let is_empty stream =
    match stream.decoder_state with
    | IncompleteHeader ->
        stream.fragments_size = 0
    | _ ->
        false

  let rec get_next_frame stream =
    match stream.decoder_state with
    | IncompleteHeader -> unpack_header stream
    | IncompleteFrame incomplete_frame -> unpack_frame stream incomplete_frame

  and unpack_header stream =
    (* First four bytes of the header contain a segment count, which tells
       you how long the full header is *)
    match peek_bytes stream 4 with
    | Some partial_header ->
        let segment_count_u32 = StringStorage.get_uint32 partial_header 0 in
        begin try
          let segment_count = 1 + (Uint32.to_int segment_count_u32) in
          let frame_header_size =
            let word_size = 8 in
            (Util.ceil_int (4 * (segment_count + 1)) word_size) * word_size
          in
          (* Now we know the full header, so try to get the whole thing *)
          begin match remove_bytes stream frame_header_size with
          | Some frame_header ->
              let () = stream.decoder_state <- IncompleteFrame {
                  frame_header;
                  complete_segments = Res.Array.empty ();
                }
              in
              get_next_frame stream
          | None ->
              Result.Error FramingError.Incomplete
          end
        with Invalid_argument _ ->
          Result.Error FramingError.Unsupported
        end
    | None ->
        Result.Error FramingError.Incomplete

  and unpack_frame stream incomplete_frame =
    let segment_count_u32 = StringStorage.get_uint32 incomplete_frame.frame_header 0 in
    let segment_count = 1 + (Uint32.to_int segment_count_u32) in
    let segments_decoded = Res.Array.length incomplete_frame.complete_segments in
    if segments_decoded = segment_count then
      let () = stream.decoder_state <- IncompleteHeader in
      Result.Ok (Res.Array.to_list incomplete_frame.complete_segments)
    else
      let () = assert (segments_decoded < segment_count) in
      let segment_size_words_u32 = StringStorage.get_uint32
          incomplete_frame.frame_header (4 + (4 * segments_decoded))
      in
      begin try
        let segment_size = 8 * (Uint32.to_int segment_size_words_u32) in
        begin match remove_bytes stream segment_size with
        | Some segment ->
            let () = Res.Array.add_one incomplete_frame.complete_segments segment in
            unpack_frame stream incomplete_frame
        | None ->
            Result.Error FramingError.Incomplete
        end
      with Invalid_argument _ ->
        Result.Error FramingError.Unsupported
      end

end


