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


(** Pack a word_aligned string. *)
let pack_string (s : string) : string =
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
        (Bytes.unsafe_to_string output_buf) 0 output_count
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
    if ofs + 8 > String.length s || count = 0xff ||
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
    if ofs + 8 > String.length s || count = 0xff then
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


(** Unpack as much data as possible from the packed fragment buffer.
    into the unpacked fragment buffer.

    Note: this might be kind of slow due to heavy reliance on higher-order
    functions with closures.  A less-elegant-but-faster implementation could
    be worthwhile. *)
let unpack ~(packed : FragmentBuffer.t) ~(unpacked : FragmentBuffer.t) : unit =
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
        let unconsumed_bytes = Util.str_slice ~start:ofs buf in
        let bytes_missing =
          required_byte_count - (String.length unconsumed_bytes)
        in
        match FragmentBuffer.remove_at_least packed bytes_missing with
        | Some fragment ->
            unpack_aux (unconsumed_bytes ^ fragment) 0
        | None ->
            FragmentBuffer.unremove packed unconsumed_bytes
    in
    try_apply ~required_byte_count:1 ~f:begin fun () ->
      match buf.[ofs] with
      | '\x00' ->
          (* Followed by a count byte specifying number of zero words - 1 *)
          try_apply ~required_byte_count:2 ~f:(fun () ->
            let zero_word_count = 1 + (Char.to_int buf.[ofs + 1]) in
            let zeros = String.make (zero_word_count * 8) '\x00' in
            let () = FragmentBuffer.add_fragment unpacked zeros in
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
                let () = FragmentBuffer.add_fragment unpacked first_literal_word in
                let () = FragmentBuffer.add_fragment unpacked other_literal_words in
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
                  let () = FragmentBuffer.add_fragment unpacked
                      (Bytes.unsafe_to_string output_word) in
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


