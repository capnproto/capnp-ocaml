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


module FragmentBuffer : sig
  type t

  (** Create a new, empty fragment buffer. *)
  val empty : unit -> t

  (** Create a new fragment buffer containing the contents of the string. *)
  val of_string : string -> t

  (** Get the number of bytes stored in the fragment buffer. *)
  val byte_count : t -> int

  (** Add a fragment to the back of the fragment buffer. *)
  val add_fragment : t -> string -> unit

  (** Remove a specific number of bytes from the front of the fragment buffer. *)
  val remove_exact : t -> int -> string option

  (** Remove at least the specified number of bytes from the front of the
      fragment buffer.  This is a less expensive operation than [remove_exact]. *)
  val remove_at_least : t -> int -> string option

  (** Examine a specific number of bytes from the front of the fragment buffer,
      without removing the data. *)
  val peek_exact : t -> int -> string option

  (** Return some bytes to the front of the fragment buffer. *)
  val unremove : t -> string -> unit

end = struct

  type t = {
    (** String fragments stored in FIFO order *)
    fragments : string Dequeue.t;

    (** Total byte count of the fragments *)
    mutable fragments_size : int;
  }

  let empty () = {
    fragments = Dequeue.create ();
    fragments_size = 0;
  }

  let add_fragment stream fragment =
    let len = String.length fragment in
    if len = 0 then
      ()
    else
      let () = Dequeue.enqueue_back stream.fragments fragment in
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
          let fragment = Dequeue.dequeue_front_exn stream.fragments in
          let bytes_from_fragment = min bytes_remaining (String.length fragment) in
          Bytes.blit
            (Bytes.of_string fragment) 0
            buf !ofs
            bytes_from_fragment;
          begin if bytes_from_fragment < String.length fragment then
            let remainder = Util.str_slice ~start:bytes_from_fragment fragment in
            Dequeue.enqueue_front stream.fragments remainder
          end;
          ofs := !ofs + bytes_from_fragment;
        done;
        stream.fragments_size <- stream.fragments_size - size;
      in
      Some (Bytes.to_string buf)

  let remove_at_least stream size =
    if stream.fragments_size < size then
      None
    else begin
      let buffer = Buffer.create size in
      while Buffer.length buffer < size do
        Buffer.add_string buffer (Dequeue.dequeue_front_exn stream.fragments)
      done;
      Some (Buffer.contents buffer)
    end

  let peek_exact stream size =
    match remove_exact stream size with
    | Some bytes ->
        let () = Dequeue.enqueue_front stream.fragments bytes in
        let () = stream.fragments_size <- stream.fragments_size + size in
        Some bytes
    | None ->
        None

  let unremove stream bytes =
    let len = String.length bytes in
    if len = 0 then
      ()
    else
      let () = Dequeue.enqueue_front stream.fragments bytes in
      stream.fragments_size <- stream.fragments_size + len

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
        let segment_count_u32 = BytesStorage.get_uint32 partial_header 0 in
        begin try
          let segment_count = 1 + (Uint32.to_int segment_count_u32) in
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
        with Invalid_argument _ ->
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

let bits_set_lookup =
  let count_bits x =
    let rec loop sum i =
      if i = 8 then
        sum
      else
        if (x land (1 lsl i)) <> 0 then
          loop (sum + 1) (i + 1)
        else
          loop sum (i + 1)
    in
    loop 0 0
  in
  let table = Array.create 256 0 in
  for i = 0 to Array.length table - 1 do
    table.(i) <- count_bits i
  done;
  table

let bits_set x = bits_set_lookup.(x)


module PackedStream = struct
  type t = {
    (** Packed fragments waiting to be unpacked *)
    packed : FragmentBuffer.t;

    (** Unpacked fragments waiting to be decoded as messages *)
    unpacked : FramedStream.t;
  }

  (** Unpack as much data as possible from the packed fragment buffer.

      Note: this might be kind of slow due to heavy reliance on higher-order
      functions with closures.  A less-elegant-but-faster implementation could
      be worthwhile. *)
  let unpack stream =
    let rec unpack_aux buf ofs =
      (* If at least [required_byte_count] bytes are available in the buffer,
         then [f ()] is returned.  Otherwise, if the required number of bytes
         can be obtained from the fragment buffer, then grab those bytes
         and recurse into [unpack_aux] with a now-larger buffer.  Otherwise,
         give back the buffer and stop. *)
      let try_apply ~required_byte_count ~f =
        if String.length buf >= required_byte_count + ofs then
          f ()
        else
          let unconsumed_bytes = Util.str_slice ~stop:ofs buf in
          let bytes_missing =
            required_byte_count - (String.length unconsumed_bytes)
          in
          match FragmentBuffer.remove_at_least stream.packed bytes_missing with
          | Some fragment ->
              unpack_aux (unconsumed_bytes ^ fragment) 0
          | None ->
              FragmentBuffer.unremove stream.packed unconsumed_bytes
      in
      try_apply ~required_byte_count:1 ~f:begin fun () ->
        match buf.[ofs] with
        | '\x00' ->
            (* Followed by a count byte specifying number of zero words - 1 *)
            try_apply ~required_byte_count:2 ~f:(fun () ->
              let zero_word_count = 1 + (Char.to_int buf.[ofs + 1]) in
              let zeros = String.make (zero_word_count * 8) '\x00' in
              let () = FramedStream.add_fragment stream.unpacked zeros in
              unpack_aux buf (ofs + 2))
        | '\xff' ->
            (* Followed by 8 literal bytes, followed by count byte *)
            try_apply ~required_byte_count:10 ~f:(fun () ->
              (* The count byte specifies number of literal words to copy *)
              let extra_bytes_required = 8 * (Char.to_int buf.[ofs + 9]) in
              try_apply ~required_byte_count:(10 + extra_bytes_required)
                ~f:(fun () ->
                  let first_literal_word =
                    Util.str_slice ~start:(ofs + 1) ~stop:(ofs + 9) buf
                  in
                  let other_literal_words =
                    Util.str_slice ~start:(ofs + 10)
                      ~stop:(ofs + 10 + extra_bytes_required) buf
                  in
                  let () = FramedStream.add_fragment stream.unpacked
                      (first_literal_word ^ other_literal_words)
                  in
                  unpack_aux buf (ofs + 10 + extra_bytes_required)))
        | c ->
            (* Followed by one literal byte for every bit set *)
            let c_int = Char.to_int c in
            let literal_bytes_required = bits_set c_int in
            try_apply ~required_byte_count:(1 + literal_bytes_required)
              ~f:(fun () ->
                let src_ofs = ref (ofs + 1) in
                let output_word = Bytes.create 8 in
                let rec loop i =
                  if i = 8 then
                    let () = FramedStream.add_fragment stream.unpacked
                        (Bytes.to_string output_word) in
                    unpack_aux buf !src_ofs
                  else
                    let () =
                      if (c_int land (1 lsl i)) <> 0 then begin
                        Bytes.set output_word i buf.[!src_ofs];
                        src_ofs := !src_ofs + 1
                      end else
                        Bytes.set output_word i '\x00'
                    in
                    loop (i + 1)
                in
                loop 0)
      end
    in
    unpack_aux "" 0


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

  let is_empty stream =
    (FragmentBuffer.byte_count stream.packed = 0) &&
    (FramedStream.is_empty stream.unpacked)

  let get_next_frame stream =
    let () = unpack stream in
    FramedStream.get_next_frame stream.unpacked

end


let make_header message =
  let buf = Buffer.create 8 in
  let () = List.iter message ~f:(fun segment ->
      let size_buf = Bytes.create 4 in
      let () = BytesStorage.set_uint32 size_buf 0
          (Uint32.of_int (String.length segment))
      in
      Buffer.add_string buf (Bytes.to_string size_buf))
  in
  let segment_sizes = Buffer.contents buf in
  let segment_count = (String.length segment_sizes) / 4 in
  if segment_count = 0 then
    invalid_arg "make_header requires nonempty message"
  else
    let count_buf = Bytes.create 4 in
    let () = BytesStorage.set_uint32 count_buf 0
        (Uint32.of_int (segment_count - 1))
    in
    (* pad out to a word boundary *)
    let count_buf = Bytes.to_string count_buf in
    if segment_count mod 2 = 0 then
      count_buf ^ segment_sizes ^ (String.make 4 '\x00')
    else
      count_buf ^ segment_sizes


let serialize_fold message ~init ~f =
  let header = make_header message in
  List.fold_left message ~init:(f init header) ~f


let serialize_iter message ~f =
  serialize_fold message ~init:() ~f:(fun () s -> f s)


let serialize message =
  (make_header message) ^ (String.concat ~sep:"" message)


let count_zeros ~start ~stop s =
  let sum = ref 0 in
  for i = start to stop - 1 do
    if s.[i] = '\x00' then
      sum := !sum + 1
    else
      ()
  done;
  !sum


let pack_string s =
  (* String should be padded out to a word boundary *)
  let () = assert ((String.length s) mod 8 = 0) in
  let buf = Buffer.create ((String.length s) / 4) in

  (* Main loop across words of the message *)
  let rec loop_words ofs =
    let () = assert (ofs <= String.length s) in
    if ofs = String.length s then
      Buffer.contents buf
    else
      loop_bytes ~tag_byte:0 ~output_buf:(Bytes.create 9) ~output_count:1
        ~ofs 0

  (* Loop across bytes within a word, emitting a tag byte followed by
     a variable number of literal bytes *)
  and loop_bytes ~tag_byte ~output_buf ~output_count ~ofs bit =
    if bit = 8 then
      let () = Bytes.set output_buf 0 (Char.of_int_exn tag_byte) in
      let () = Buffer.add_substring buf
        (Bytes.to_string output_buf) 0 output_count
      in
      if tag_byte = 0 then
        loop_zero_words ~count:0 (ofs + 8)
      else if tag_byte = 0xff then
        loop_literal_words ~count:0 ~lit_buf:(Buffer.create 128) (ofs + 8)
      else
        loop_words (ofs + 8)
    else
      if s.[ofs + bit] = '\x00' then
        loop_bytes ~tag_byte ~output_buf ~output_count ~ofs (bit + 1)
      else
        let () = Bytes.set output_buf output_count s.[ofs + bit] in
        loop_bytes
          ~tag_byte:(tag_byte lor (1 lsl bit))
          ~output_buf ~output_count:(output_count + 1)
          ~ofs (bit + 1)

  (* Emitting a run of zeros *)
  and loop_zero_words ~count ofs =
    let () = assert (ofs <= String.length s) in
    if ofs + 8 >= String.length s || count = 0xff ||
       count_zeros ~start:ofs ~stop:(ofs + 8) s <> 8 then
      let () = Buffer.add_char buf (Char.of_int_exn count) in
      loop_words ofs
    else
      loop_zero_words ~count:(count + 1) (ofs + 8)

  (* Emitting a run of literal bytes.  This algorithm is only using
     a single word of lookahead, which is fast but will not optimize
     packing efficiency. *)
  and loop_literal_words ~count ~lit_buf ofs =
    let () = assert (ofs <= String.length s) in
    if ofs + 8 >= String.length s || count = 0xff then
      let () = Buffer.add_char buf (Char.of_int_exn count) in
      let () = Buffer.add_buffer buf lit_buf in
      loop_words ofs
    else
      let num_zeros = count_zeros ~start:ofs ~stop:(ofs + 8) s in
      if num_zeros < 3 then
        let () = Buffer.add_substring lit_buf s ofs 8 in
        loop_literal_words ~count:(count + 1) ~lit_buf (ofs + 8)
      else
        let () = Buffer.add_char buf (Char.of_int_exn count) in
        let () = Buffer.add_buffer buf lit_buf in
        loop_words ofs
  in
  loop_words 0


let pack_fold message ~init ~f =
  serialize_fold message ~init
    ~f:(fun acc unpacked_fragment -> f acc (pack_string unpacked_fragment))

let pack_iter message ~f =
  pack_fold message ~init:() ~f:(fun () s -> f s)

let pack message =
  pack_string (serialize message)

