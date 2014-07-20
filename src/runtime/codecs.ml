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
  type t = CodecsSig.FramingError.t =
    | Incomplete    (** less than a full frame is available *)
    | Unsupported   (** frame header describes a segment count or segment size that
                        is too large for the implementation *)
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
    fragment_buffer : FragmentBuffer.t;

    (** Partially-decoded frame information *)
    mutable decoder_state : decoder_state_t;
  }

  let empty () = {
    fragment_buffer = FragmentBuffer.empty ();
    decoder_state = IncompleteHeader;
  }

  let of_string s = {
    fragment_buffer = FragmentBuffer.of_string s;
    decoder_state = IncompleteHeader;
  }

  let add_fragment stream fragment =
    FragmentBuffer.add_fragment stream.fragment_buffer fragment

  let bytes_available stream =
    match stream.decoder_state with
    | IncompleteHeader ->
        FragmentBuffer.byte_count stream.fragment_buffer
    | IncompleteFrame partial_frame ->
        (String.length partial_frame.frame_header) +
        (Res.Array.fold_left (fun acc x -> acc + (String.length x))
           0
           partial_frame.complete_segments) +
        (FragmentBuffer.byte_count stream.fragment_buffer)

  let is_empty stream =
    match stream.decoder_state with
    | IncompleteHeader ->
        FragmentBuffer.byte_count stream.fragment_buffer = 0
    | _ ->
        false

  let rec get_next_frame stream =
    match stream.decoder_state with
    | IncompleteHeader -> unpack_header stream
    | IncompleteFrame incomplete_frame -> unpack_frame stream incomplete_frame

  and unpack_header stream =
    (* First four bytes of the header contain a segment count, which tells
       you how long the full header is *)
    match FragmentBuffer.peek_exact stream.fragment_buffer 4 with
    | Some partial_header ->
        begin try
          let segment_count =
            Util.int_of_uint32_exn (BytesStorage.get_uint32 partial_header 0)
          in
          let () =
            if segment_count > (Int.max_value / 4) - 2 then
              Util.out_of_int_range "Uint32.to_int"
          in
          let segment_count = segment_count + 1 in
          let frame_header_size =
            let word_size = 8 in
            (Util.ceil_ratio (4 * (segment_count + 1)) word_size) * word_size
          in
          (* Now we know the full header, so try to get the whole thing *)
          begin match FragmentBuffer.remove_exact stream.fragment_buffer
                        frame_header_size with
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
        with Util.Out_of_int_range _ ->
          Result.Error FramingError.Unsupported
        end
    | None ->
        Result.Error FramingError.Incomplete

  and unpack_frame stream incomplete_frame =
    let segment_count_u32 = BytesStorage.get_uint32 incomplete_frame.frame_header 0 in
    let segment_count = 1 + (Uint32.to_int segment_count_u32) in
    let segments_decoded = Res.Array.length incomplete_frame.complete_segments in
    if segments_decoded = segment_count then
      let () = stream.decoder_state <- IncompleteHeader in
      Result.Ok (Res.Array.to_list incomplete_frame.complete_segments)
    else
      let () = assert (segments_decoded < segment_count) in
      let segment_size_words_u32 = BytesStorage.get_uint32
          incomplete_frame.frame_header (4 + (4 * segments_decoded))
      in
      begin try
        let segment_size = 8 * (Uint32.to_int segment_size_words_u32) in
        begin match FragmentBuffer.remove_exact stream.fragment_buffer
                      segment_size with
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


module PackedStream = struct

  type t = {
    (** Packed fragments waiting to be unpacked *)
    packed : FragmentBuffer.t;

    (** Unpacked fragments waiting to be decoded as messages *)
    unpacked : FramedStream.t;
  }

  let empty () = {
    packed = FragmentBuffer.empty ();
    unpacked = FramedStream.empty ();
  }

  let of_string s = {
    packed = FragmentBuffer.of_string s;
    unpacked = FramedStream.empty ();
  }

  let add_fragment stream fragment =
    FragmentBuffer.add_fragment stream.packed fragment

  let bytes_available stream =
    (* This isn't a very meaningful number, except maybe for the
       purpose of bounding the amount of memory in use... *)
    (FragmentBuffer.byte_count stream.packed) +
    (FramedStream.bytes_available stream.unpacked)

  let is_empty stream =
    (FragmentBuffer.byte_count stream.packed = 0) &&
    (FramedStream.is_empty stream.unpacked)

  let get_next_frame stream =
    let () = Packing.unpack ~packed:stream.packed
        ~unpacked:stream.unpacked.FramedStream.fragment_buffer
    in
    FramedStream.get_next_frame stream.unpacked

end


let make_header segments =
  let buf = Buffer.create 8 in
  let () = List.iter segments ~f:(fun segment ->
      let size_buf = Bytes.create 4 in
      let seg_len = Bytes.length segment in
      let () = assert ((seg_len mod 8) = 0) in
      let seg_word_count = seg_len / 8 in
      let () = BytesStorage.set_uint32 size_buf 0
          (Util.uint32_of_int_exn seg_word_count)
      in
      Buffer.add_string buf (Bytes.unsafe_to_string size_buf))
  in
  let segment_sizes = Buffer.contents buf in
  let segment_count = (String.length segment_sizes) / 4 in
  if segment_count = 0 then
    invalid_arg "make_header requires nonempty message"
  else
    let count_buf = Bytes.create 4 in
    let () = BytesStorage.set_uint32 count_buf 0
        (Util.uint32_of_int_exn (segment_count - 1))
    in
    (* pad out to a word boundary *)
    let count_buf = Bytes.unsafe_to_string count_buf in
    if segment_count mod 2 = 0 then
      count_buf ^ segment_sizes ^ (String.make 4 '\x00')
    else
      count_buf ^ segment_sizes


let serialize_fold message ~init ~f =
  let segments = Message.BytesMessage.Message.to_storage message in
  let header = make_header segments in
  List.fold_left segments ~init:(f init header) ~f


let serialize_iter message ~f =
  serialize_fold message ~init:() ~f:(fun () s -> f s)


let serialize message =
  let segments = Message.BytesMessage.Message.to_storage message in
  (make_header segments) ^ (String.concat ~sep:"" segments)


let pack_fold message ~init ~f =
  serialize_fold message ~init
    ~f:(fun acc unpacked_fragment -> f acc (Packing.pack_string unpacked_fragment))

let pack_iter message ~f =
  pack_fold message ~init:() ~f:(fun () s -> f s)

let pack message =
  Packing.pack_string (serialize message)

