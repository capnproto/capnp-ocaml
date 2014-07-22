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

let count_zeros ~start ~stop s =
  let sum = ref 0 in
  for i = start to stop - 1 do
    if s.[i] = '\x00' then
      sum := !sum + 1
    else
      ()
  done;
  !sum


(* Main loop across words of the message *)
let rec pack_loop_words ~input ~ofs ~buf =
  let () = assert (ofs <= String.length input) in
  if ofs = String.length input then
    Buffer.contents buf
  else
    pack_loop_bytes ~input ~ofs ~buf


(* Loop across bytes within a word, emitting a tag byte followed by
   a variable number of literal bytes *)
and pack_loop_bytes ~input ~ofs ~buf =
  let output_buf = Bytes.create 9 in
  let tag_byte = ref 0 in
  let output_count = ref 1 in
  for bit = 0 to 7 do
    let c = input.[ofs + bit] in
    if c = '\x00' then
      ()
    else begin
      let () = Bytes.set output_buf !output_count c in
      tag_byte := !tag_byte lor (1 lsl bit);
      output_count := !output_count + 1
    end
  done;
  let () = Bytes.set output_buf 0 (Char.of_int_exn !tag_byte) in
  let () = Buffer.add_substring buf
    (Bytes.unsafe_to_string output_buf) 0 !output_count
  in
  if !tag_byte = 0 then
    pack_loop_zero_words ~input ~ofs:(ofs + 8) ~count:0 ~buf
  else if !tag_byte = 0xff then
    pack_loop_literal_words ~input ~ofs:(ofs + 8) ~buf ~count:0
      ~lit_buf:(Buffer.create 128)
  else
    pack_loop_words ~input ~ofs:(ofs + 8) ~buf


(* Emitting a run of zeros *)
and pack_loop_zero_words ~input ~ofs ~count ~buf =
  let () = assert (ofs <= String.length input) in
  if ofs + 8 > String.length input || count = 0xff ||
     count_zeros ~start:ofs ~stop:(ofs + 8) input <> 8 then
    let () = Buffer.add_char buf (Char.of_int_exn count) in
    pack_loop_words ~input ~ofs ~buf
  else
    pack_loop_zero_words ~input ~ofs:(ofs + 8) ~count:(count + 1) ~buf


(* Emitting a run of literal bytes.  This algorithm is only using
   a single word of lookahead, which is fast but will not optimize
   packing efficiency. *)
and pack_loop_literal_words ~input ~ofs ~buf ~count ~lit_buf =
  let () = assert (ofs <= String.length input) in
  if ofs + 8 > String.length input || count = 0xff then
    let () = Buffer.add_char buf (Char.of_int_exn count) in
    let () = Buffer.add_buffer buf lit_buf in
    pack_loop_words ~input ~ofs ~buf
  else
    let num_zeros = count_zeros ~start:ofs ~stop:(ofs + 8) input in
    if num_zeros < 3 then
      let () = Buffer.add_substring lit_buf input ofs 8 in
      pack_loop_literal_words ~input ~ofs:(ofs + 8) ~buf ~count:(count + 1) ~lit_buf
    else
      let () = Buffer.add_char buf (Char.of_int_exn count) in
      let () = Buffer.add_buffer buf lit_buf in
      pack_loop_words ~input ~ofs ~buf


(** Pack a word_aligned string. *)
let pack_string (s : string) : string =
  (* String should be padded out to a word boundary *)
  let () = assert ((String.length s) mod 8 = 0) in
  let buf = Buffer.create ((String.length s) / 4) in
  pack_loop_words ~input:s ~ofs:0 ~buf


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


(* Consider coalescing the remaining bytes of [buf] with the next fragment in
   the buffer. If this is large enough to satisfy the [required_bytes_count],
   then proceed with [unpack_decode_tag].  Otherwise, put the remainder of
   the buffer back into [packed] and give up. *)
let rec unpack_coalesce_buffer_and_retry ~packed ~unpacked ~buf ~ofs
    ~required_byte_count : unit =
  let unconsumed_bytes = Util.str_slice ~start:ofs buf in
  let bytes_missing =
    required_byte_count - (String.length unconsumed_bytes)
  in
  match FragmentBuffer.remove_at_least packed bytes_missing with
  | Some fragment ->
      unpack_decode_tag ~packed ~unpacked ~buf:(unconsumed_bytes ^ fragment) ~ofs:0
  | None ->
      FragmentBuffer.unremove packed unconsumed_bytes


(* Decode a tag byte and then attempt to decode all the bytes that are
   associated with the tag. *)
and unpack_decode_tag ~packed ~unpacked ~buf ~ofs =
  if ofs = String.length buf then
    unpack_coalesce_buffer_and_retry ~packed ~unpacked ~buf ~ofs ~required_byte_count:1
  else
    match buf.[ofs] with
    | '\x00' ->
        (* Followed by a count byte specifying number of zero words - 1 *)
        let required_byte_count = 2 in
        if String.length buf - ofs >= required_byte_count then
          let zero_word_count = 1 + (Char.to_int buf.[ofs + 1]) in
          let zeros = String.make (zero_word_count * 8) '\x00' in
          let () = FragmentBuffer.add_fragment unpacked zeros in
          unpack_decode_tag ~packed ~unpacked ~buf ~ofs:(ofs + 2)
        else
          unpack_coalesce_buffer_and_retry ~packed ~unpacked ~buf ~ofs
            ~required_byte_count
    | '\xff' ->
        (* Followed by 8 literal bytes, followed by count byte *)
        let required_byte_count = 10 in
        if String.length buf - ofs >= required_byte_count then
          (* The count byte specifies number of literal words to copy *)
          let extra_bytes_required = 8 * (Char.to_int buf.[ofs + 9]) in
          let required_byte_count' = required_byte_count + extra_bytes_required in
          if String.length buf - ofs >= required_byte_count' then
            let first_literal_word =
              Util.str_slice ~start:(ofs + 1) ~stop:(ofs + 9) buf
            in
            let other_literal_words =
              Util.str_slice ~start:(ofs + 10)
                ~stop:(ofs + 10 + extra_bytes_required) buf
            in
            let () = FragmentBuffer.add_fragment unpacked first_literal_word in
            let () = FragmentBuffer.add_fragment unpacked other_literal_words in
            unpack_decode_tag ~packed ~unpacked ~buf ~ofs:(ofs + 10 + extra_bytes_required)
          else
            unpack_coalesce_buffer_and_retry ~packed ~unpacked ~buf ~ofs
              ~required_byte_count:required_byte_count'
        else
          unpack_coalesce_buffer_and_retry ~packed ~unpacked ~buf ~ofs
            ~required_byte_count
    | c ->
        (* Followed by one literal byte for every bit set *)
        let c_int = Char.to_int c in
        let literal_bytes_required = bits_set c_int in
        let required_byte_count = 1 + literal_bytes_required in
        if String.length buf - ofs >= required_byte_count then begin
          let src_ofs = ref (ofs + 1) in
          let output_word = Bytes.create 8 in
          for i = 0 to 7 do
            if (c_int land (1 lsl i)) <> 0 then begin
              Bytes.set output_word i buf.[!src_ofs];
              src_ofs := !src_ofs + 1
            end else
              Bytes.set output_word i '\x00'
          done;
          let () = FragmentBuffer.add_fragment unpacked
              (Bytes.unsafe_to_string output_word)
          in
          unpack_decode_tag ~packed ~unpacked ~buf ~ofs:!src_ofs
        end else
          unpack_coalesce_buffer_and_retry ~packed ~unpacked ~buf ~ofs
            ~required_byte_count


(** Unpack as much data as possible from the packed fragment buffer.
    into the unpacked fragment buffer. *)
let unpack ~(packed : FragmentBuffer.t) ~(unpacked : FragmentBuffer.t) : unit =
  unpack_decode_tag ~packed ~unpacked ~buf:"" ~ofs:0


(* Provided for testing purposes only. *)
let unpack_string (s : string) : string =
  let packed = FragmentBuffer.of_string s in
  let unpacked = FragmentBuffer.empty () in
  let () = unpack ~packed ~unpacked in
  let () = assert (FragmentBuffer.byte_count packed = 0) in
  let bytes_avail = FragmentBuffer.byte_count unpacked in
  let result =
    match FragmentBuffer.remove_at_least unpacked bytes_avail with
    | Some v -> v
    | None -> assert false
  in
  let () = assert (FragmentBuffer.byte_count unpacked = 0) in
  let () = assert (String.length result = bytes_avail) in
  result


