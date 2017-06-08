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


let count_zeros ~start ~stop s =
  let sum = ref 0 in
  for i = start to stop - 1 do
    if s.[i] = '\x00' then
      sum := !sum + 1
    else
      ()
  done;
  !sum


(* Persistent buffers for use with [pack_loop_XXX].  Using persistent
   buffers reduces GC pressure. *)
type output_buffers_t = {
  (* Collects compressed output *)
  output_buf : Buffer.t;

  (* Collects literals during pack_loop_literal_words *)
  lit_buf : Buffer.t;

  (* Collects groups of tagged bytes during pack_loop_bytes *)
  tagged_bytes : Bytes.t;
}


(* Main loop across words of the message *)
let rec pack_loop_words ~input ~ofs ~buffers =
  let () = assert (ofs <= String.length input) in
  if ofs = String.length input then
    Buffer.contents buffers.output_buf
  else
    pack_loop_bytes ~input ~ofs ~buffers


(* Loop across bytes within a word, emitting a tag byte followed by
   a variable number of literal bytes *)
and pack_loop_bytes ~input ~ofs ~buffers =
  let tag_byte = ref 0 in
  let output_count = ref 1 in

  (* This is a hot loop.  Unrolling is good for ~20% speedup. *)

  let () =
    let c = String.unsafe_get input ofs in
    if c <> '\x00' then begin
      Bytes.unsafe_set buffers.tagged_bytes !output_count c;
      tag_byte := !tag_byte lor 0x1;
      output_count := !output_count + 1
    end
  in
  let () =
    let c = String.unsafe_get input (ofs + 1) in
    if c <> '\x00' then begin
      Bytes.unsafe_set buffers.tagged_bytes !output_count c;
      tag_byte := !tag_byte lor 0x2;
      output_count := !output_count + 1
    end
  in
  let () =
    let c = String.unsafe_get input (ofs + 2) in
    if c <> '\x00' then begin
      Bytes.unsafe_set buffers.tagged_bytes !output_count c;
      tag_byte := !tag_byte lor 0x4;
      output_count := !output_count + 1
    end
  in
  let () =
    let c = String.unsafe_get input (ofs + 3) in
    if c <> '\x00' then begin
      Bytes.unsafe_set buffers.tagged_bytes !output_count c;
      tag_byte := !tag_byte lor 0x8;
      output_count := !output_count + 1
    end
  in
  let () =
    let c = String.unsafe_get input (ofs + 4) in
    if c <> '\x00' then begin
      Bytes.unsafe_set buffers.tagged_bytes !output_count c;
      tag_byte := !tag_byte lor 0x10;
      output_count := !output_count + 1
    end
  in
  let () =
    let c = String.unsafe_get input (ofs + 5) in
    if c <> '\x00' then begin
      Bytes.unsafe_set buffers.tagged_bytes !output_count c;
      tag_byte := !tag_byte lor 0x20;
      output_count := !output_count + 1
    end
  in
  let () =
    let c = String.unsafe_get input (ofs + 6) in
    if c <> '\x00' then begin
      Bytes.unsafe_set buffers.tagged_bytes !output_count c;
      tag_byte := !tag_byte lor 0x40;
      output_count := !output_count + 1
    end
  in
  let () =
    let c = String.unsafe_get input (ofs + 7) in
    if c <> '\x00' then begin
      Bytes.unsafe_set buffers.tagged_bytes !output_count c;
      tag_byte := !tag_byte lor 0x80;
      output_count := !output_count + 1
    end
  in

  let () = Bytes.unsafe_set buffers.tagged_bytes 0 (Char.unsafe_chr !tag_byte) in
  let () = Buffer.add_substring buffers.output_buf
    (Bytes.unsafe_to_string buffers.tagged_bytes) 0 !output_count
  in
  if !tag_byte = 0 then
    pack_loop_zero_words ~input ~ofs:(ofs + 8) ~buffers ~count:0
  else if !tag_byte = 0xff then
    let () = Buffer.clear buffers.lit_buf in
    pack_loop_literal_words ~input ~ofs:(ofs + 8) ~buffers ~count:0
  else
    pack_loop_words ~input ~ofs:(ofs + 8) ~buffers


(* Emitting a run of zeros *)
and pack_loop_zero_words ~input ~ofs ~count ~buffers =
  let () = assert (ofs <= String.length input) in
  if ofs + 8 > String.length input || count = 0xff ||
     count_zeros ~start:ofs ~stop:(ofs + 8) input <> 8 then
    let () = Buffer.add_char buffers.output_buf (Char.chr count) in
    pack_loop_words ~input ~ofs ~buffers
  else
    pack_loop_zero_words ~input ~ofs:(ofs + 8) ~count:(count + 1) ~buffers


(* Emitting a run of literal bytes.  This algorithm is only using
   a single word of lookahead, which is fast but will not optimize
   packing efficiency. *)
and pack_loop_literal_words ~input ~ofs ~buffers ~count =
  let () = assert (ofs <= String.length input) in
  if ofs + 8 > String.length input || count = 0xff then
    let () = Buffer.add_char buffers.output_buf (Char.chr count) in
    let () = Buffer.add_buffer buffers.output_buf buffers.lit_buf in
    pack_loop_words ~input ~ofs ~buffers
  else
    let num_zeros = count_zeros ~start:ofs ~stop:(ofs + 8) input in
    if num_zeros < 3 then
      let () = Buffer.add_substring buffers.lit_buf input ofs 8 in
      pack_loop_literal_words ~input ~ofs:(ofs + 8) ~buffers ~count:(count + 1)
    else
      let () = Buffer.add_char buffers.output_buf (Char.chr count) in
      let () = Buffer.add_buffer buffers.output_buf buffers.lit_buf in
      pack_loop_words ~input ~ofs ~buffers


(** Pack a word_aligned string. *)
let pack_string (s : string) : string =
  (* String should be padded out to a word boundary *)
  let () = assert ((String.length s) mod 8 = 0) in
  let buffers = {
    output_buf   = Buffer.create ((String.length s) / 4);
    lit_buf      = Buffer.create 128;
    tagged_bytes = Bytes.create 9;
  } in
  pack_loop_words ~input:s ~ofs:0 ~buffers


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
  let table = Array.make 256 0 in
  for i = 0 to Array.length table - 1 do
    table.(i) <- count_bits i
  done;
  table

let bits_set x = bits_set_lookup.(x)


(* Persistent buffers used while unpacking sequences of mixed bytes.
   Using persistent buffers reduces GC pressure. *)
module MixedContext = struct
  type t = {
    (* Sequences of mixed bytes are stored in this temporary buffer,
       and transferred to the deque-based FragmentBuffer only when
       necessary.  This approach avoids filling the FragmentBuffer with
       lots of small strings. *)
    fragments : Buffer.t;

    (* Reusable buffer which stores a single word as it is unpacked. *)
    word : Bytes.t;
  }
end


(* Move a series of mixed bytes from a temporary Buffer.t into the
   FragmentBuffer. *)
let transfer_mixed_bytes ~unpacked ~mixed_context =
  FragmentBuffer.add_fragment unpacked
    (Buffer.contents mixed_context.MixedContext.fragments);
  Buffer.clear mixed_context.MixedContext.fragments


(* Consider coalescing the remaining bytes of [buf] with the next fragment in
   the buffer. If this is large enough to satisfy the [required_bytes_count],
   then proceed with [unpack_decode_tag].  Otherwise, put the remainder of
   the buffer back into [packed] and give up. *)
let rec unpack_coalesce_buffer_and_retry ~packed ~unpacked ~buf ~ofs
    ~mixed_context ~required_byte_count : unit =
  transfer_mixed_bytes ~unpacked ~mixed_context;
  let unconsumed_bytes = Util.str_slice ~start:ofs buf in
  let bytes_missing =
    required_byte_count - (String.length unconsumed_bytes)
  in
  match FragmentBuffer.remove_at_least packed bytes_missing with
  | Some fragment ->
      unpack_decode_tag ~packed ~unpacked ~buf:(unconsumed_bytes ^ fragment)
        ~ofs:0 ~mixed_context
  | None ->
      FragmentBuffer.unremove packed unconsumed_bytes


and unpack_zeros ~packed ~unpacked ~buf ~ofs ~mixed_context =
  transfer_mixed_bytes ~unpacked ~mixed_context;
  (* Tag byte is followed by a count byte specifying number of zero words - 1 *)
  let required_byte_count = 2 in
  if String.length buf - ofs >= required_byte_count then
    let zero_word_count = 1 + (Char.code buf.[ofs + 1]) in
    let zeros = String.make (zero_word_count * 8) '\x00' in
    let () = FragmentBuffer.add_fragment unpacked zeros in
    unpack_decode_tag ~packed ~unpacked ~buf ~ofs:(ofs + 2) ~mixed_context
  else
    unpack_coalesce_buffer_and_retry ~packed ~unpacked ~buf ~ofs
      ~mixed_context ~required_byte_count


and unpack_literal_bytes ~packed ~unpacked ~buf ~ofs ~mixed_context =
  transfer_mixed_bytes ~unpacked ~mixed_context;
  (* Tag byte is followed by 8 literal bytes, followed by count byte *)
  let required_byte_count = 10 in
  if String.length buf - ofs >= required_byte_count then
    (* The count byte specifies number of literal words to copy *)
    let extra_bytes_required = 8 * (Char.code buf.[ofs + 9]) in
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
        ~mixed_context
    else
      unpack_coalesce_buffer_and_retry ~packed ~unpacked ~buf ~ofs
        ~mixed_context ~required_byte_count:required_byte_count'
  else
    unpack_coalesce_buffer_and_retry ~packed ~unpacked ~buf ~ofs
      ~mixed_context ~required_byte_count


and unpack_mixed_bytes ~packed ~unpacked ~buf ~ofs ~mixed_context ~tag =
  (* Tag byte is followed by one literal byte for every bit set *)
  let c_int = Char.code tag in
  let literal_bytes_required = bits_set c_int in
  let required_byte_count = 1 + literal_bytes_required in
  if String.length buf - ofs >= required_byte_count then begin
    let src_ofs = ref (ofs + 1) in
    Bytes.fill mixed_context.MixedContext.word 0 8 '\x00';

    (* This is the dual of the hot loop in [pack_loop_bytes]. *)

    if (c_int land 0x1) <> 0 then begin
      let c = String.unsafe_get buf !src_ofs in
      Bytes.unsafe_set mixed_context.MixedContext.word 0 c;
      src_ofs := !src_ofs + 1
    end;
    if (c_int land 0x2) <> 0 then begin
      let c = String.unsafe_get buf !src_ofs in
      Bytes.unsafe_set mixed_context.MixedContext.word 1 c;
      src_ofs := !src_ofs + 1
    end;
    if (c_int land 0x4) <> 0 then begin
      let c = String.unsafe_get buf !src_ofs in
      Bytes.unsafe_set mixed_context.MixedContext.word 2 c;
      src_ofs := !src_ofs + 1
    end;
    if (c_int land 0x8) <> 0 then begin
      let c = String.unsafe_get buf !src_ofs in
      Bytes.unsafe_set mixed_context.MixedContext.word 3 c;
      src_ofs := !src_ofs + 1
    end;
    if (c_int land 0x10) <> 0 then begin
      let c = String.unsafe_get buf !src_ofs in
      Bytes.unsafe_set mixed_context.MixedContext.word 4 c;
      src_ofs := !src_ofs + 1
    end;
    if (c_int land 0x20) <> 0 then begin
      let c = String.unsafe_get buf !src_ofs in
      Bytes.unsafe_set mixed_context.MixedContext.word 5 c;
      src_ofs := !src_ofs + 1
    end;
    if (c_int land 0x40) <> 0 then begin
      let c = String.unsafe_get buf !src_ofs in
      Bytes.unsafe_set mixed_context.MixedContext.word 6 c;
      src_ofs := !src_ofs + 1
    end;
    if (c_int land 0x80) <> 0 then begin
      let c = String.unsafe_get buf !src_ofs in
      Bytes.unsafe_set mixed_context.MixedContext.word 7 c;
      src_ofs := !src_ofs + 1
    end;

    Buffer.add_string
      mixed_context.MixedContext.fragments
      (Bytes.unsafe_to_string mixed_context.MixedContext.word);
    unpack_decode_tag ~packed ~unpacked ~buf ~ofs:!src_ofs ~mixed_context
  end else
    unpack_coalesce_buffer_and_retry ~packed ~unpacked ~buf ~ofs
      ~mixed_context ~required_byte_count


(* Decode a tag byte and then attempt to decode all the bytes that are
   associated with the tag. *)
and unpack_decode_tag ~packed ~unpacked ~buf ~ofs ~mixed_context =
  if ofs = String.length buf then
    unpack_coalesce_buffer_and_retry ~packed ~unpacked ~buf ~ofs
      ~mixed_context ~required_byte_count:1
  else
    match buf.[ofs] with
    | '\x00' ->
        unpack_zeros ~packed ~unpacked ~buf ~ofs ~mixed_context
    | '\xff' ->
        unpack_literal_bytes ~packed ~unpacked ~buf ~ofs ~mixed_context 
    | c ->
        unpack_mixed_bytes ~packed ~unpacked ~buf ~ofs ~mixed_context ~tag:c


(** Unpack as much data as possible from the packed fragment buffer.
    into the unpacked fragment buffer. *)
let unpack ~(packed : FragmentBuffer.t) ~(unpacked : FragmentBuffer.t) : unit =
  unpack_decode_tag ~packed ~unpacked ~buf:"" ~ofs:0
    ~mixed_context:{
      MixedContext.fragments = Buffer.create 1024;
      MixedContext.word = Bytes.create 8;
    }


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


