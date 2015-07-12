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

type compression_t = [ `None | `Packing ]

exception Unsupported_message_frame

module WriteContext = struct
  type 'a t = {
    (** File descriptor we're writing to *)
    fd : 'a;

    (** Compression format *)
    comp : compression_t;

    (** Function for writing to the descriptor *)
    write : 'a -> buf:string -> pos:int -> len:int -> int;

    (** Data remaining to write to the descriptor *)
    fragments : string Dequeue.t;

    (** Total number of bytes stored in [fragments] *)
    mutable fragments_size : int;

    (** Position within the first fragment where writing should begin *)
    mutable first_fragment_pos : int;
  }

  let create ~write ~compression fd = {
    fd;
    comp = compression;
    write;
    fragments = Dequeue.create ();
    fragments_size = 0;
    first_fragment_pos = 0;
  }

  let enqueue_message context message =
    Codecs.serialize_iter message ~compression:context.comp ~f:(fun buf ->
      Dequeue.enqueue_back context.fragments buf;
      context.fragments_size <- context.fragments_size + (String.length buf))

  let bytes_remaining context = context.fragments_size - context.first_fragment_pos

  let write context =
    if Dequeue.is_empty context.fragments then
      0
    else
      let first_fragment = Dequeue.peek_front_exn context.fragments in
      let first_fragment_remaining =
        String.length first_fragment - context.first_fragment_pos
      in
      let bytes_written = context.write context.fd
          ~buf:first_fragment ~pos:context.first_fragment_pos
          ~len:first_fragment_remaining
      in
      let () =
        if bytes_written = first_fragment_remaining then
          let (_ : string) = Dequeue.dequeue_front_exn context.fragments in
          let () = context.fragments_size <-
              context.fragments_size - (String.length first_fragment)
          in
          context.first_fragment_pos <- 0
        else
          context.first_fragment_pos <-
            context.first_fragment_pos + bytes_written
      in
      bytes_written

  let write_message context message =
    let () = enqueue_message context message in
    while bytes_remaining context > 0 do
      let (_ : int) = write context in
      ()
    done

end

let rec select ?(restart = true) fd =
  try
    select fd ~restart
  with Unix.Unix_error (Unix.EINTR, _, _) ->
    if restart
      then select fd ~restart
      else ()

module ReadContext = struct
  type 'a t = {
    (** File descriptor we're writing to *)
    fd : 'a;

    (** Stream format *)
    stream : Codecs.FramedStream.t;

    (** Function for reading from the descriptor *)
    read : 'a -> buf:Bytes.t -> pos:int -> len:int -> int;

    (** Persistent read buffer *)
    read_buf : Bytes.t;
  }

  let create ~read ~compression fd = {
    fd;
    stream = Codecs.FramedStream.empty compression;
    read;
    read_buf = Bytes.create (64 * 1024);    (* Size of ocaml internal Unix buffer *)
  }

  let dequeue_message context =
    match Codecs.FramedStream.get_next_frame context.stream with
    | Result.Ok message ->
        Some message
    | Result.Error Codecs.FramingError.Incomplete ->
        None
    | Result.Error Codecs.FramingError.Unsupported ->
        raise Unsupported_message_frame

  let bytes_available context =
    Codecs.FramedStream.bytes_available context.stream

  let read context =
    let bytes_read = context.read context.fd ~buf:context.read_buf
        ~pos:0 ~len:(Bytes.length context.read_buf)
    in
    if bytes_read > 0 then
      let str_buf = Bytes.unsafe_to_string context.read_buf in
      let substr = String.sub str_buf ~pos:0 ~len:bytes_read in
      let () = Codecs.FramedStream.add_fragment context.stream substr in
      bytes_read
    else
      bytes_read

  let read_message context =
    let rec loop () =
      match dequeue_message context with
      | Some message ->
          Some message
      | None ->
          let bytes_read = read context in
          if bytes_read = 0 then
            None
          else
            loop ()
    in
    loop ()

end


let create_write_context_for_fd ?(restart = true) ~compression fd =
  let unix_write fd' ~buf ~pos ~len =
    Unix.single_write fd' buf pos len
  in
  WriteContext.create ~write:unix_write ~compression fd


let create_write_context_for_channel ~compression chan =
  let chan_write chan' ~buf ~pos ~len =
    let () = Out_channel.output chan' ~buf ~pos ~len in
    len
  in
  WriteContext.create ~write:chan_write ~compression chan


let rec loop_eintr f =
  try
    f ()
  with Unix.Unix_error (Unix.EINTR, _, _) ->
    loop_eintr f


let create_read_context_for_fd ?(restart = true) ~compression fd =
  let unix_read fd' ~buf ~pos ~len =
    if restart then
      loop_eintr (fun () -> UnixLabels.read fd' ~buf ~pos ~len)
    else
      UnixLabels.read fd' ~buf ~pos ~len
  in
  ReadContext.create ~read:unix_read ~compression fd


let create_read_context_for_channel ~compression chan =
  let in_chan_read ic ~buf ~pos ~len =
    Pervasives.input ic buf pos len
  in
  ReadContext.create ~read:in_chan_read ~compression chan


let write_message_to_fd ?(restart = true) ~compression message fd =
  let context = create_write_context_for_fd ~restart ~compression fd in
  let () = WriteContext.enqueue_message context message in
  while WriteContext.bytes_remaining context > 0 do
    try
      let (_ : int) = WriteContext.write context in
      ()
    with
    | Unix.Unix_error (Unix.EAGAIN, _, _)
    | Unix.Unix_error (Unix.EWOULDBLOCK, _, _) ->
        (* Avoid burning CPU time looping on EAGAIN *)
        select fd ~restart
  done


let write_message_to_channel ~compression message chan =
  let context = create_write_context_for_channel ~compression chan in
  WriteContext.write_message context message


let write_message_to_file ?perm ~compression message filename =
  Out_channel.with_file filename ~binary:true ?perm ~f:(fun oc ->
    write_message_to_channel ~compression message oc)


let write_message_to_file_robust ?perm ~compression message filename =
  let (tmp_filename, tmp_oc) = Filename.open_temp_file filename "-tmp" in
  let () = Exn.protectx tmp_oc ~finally:Out_channel.close ~f:(fun fd ->
      let () = write_message_to_channel ~compression message fd in
      Out_channel.flush tmp_oc)
  in
  let () = Unix.rename tmp_filename filename in
  let () =
    (* [mkstemp] always creates as 0o600, so we may need to touch up permissions *)
    match perm with
    | Some perm ->
        Unix.chmod filename perm
    | None ->
        ()
  in
  ()

let read_single_message_from_fd ?(restart = true) ~compression fd =
  let context = create_read_context_for_fd ~restart ~compression fd in
  let rec read_loop () =
    try
      ReadContext.read context
    with
    | Unix.Unix_error (Unix.EAGAIN, _, _)
    | Unix.Unix_error (Unix.EWOULDBLOCK, _, _) ->
        (* Avoid burning CPU time looping on EAGAIN *)
        select fd ~restart;
        read_loop ()
  in
  let rec loop () =
    let bytes_read = read_loop () in
    if bytes_read = 0 then
      None
    else
      match ReadContext.dequeue_message context with
      | Some message ->
          Some message
      | None ->
          loop ()
  in
  loop ()


let read_single_message_from_channel ~compression chan =
  let context = create_read_context_for_channel ~compression chan in
  let rec loop () =
    let bytes_read = ReadContext.read context in
    if bytes_read = 0 then
      None
    else
      match ReadContext.dequeue_message context with
      | Some message ->
          Some message
      | None ->
          loop ()
  in
  loop ()


let read_message_from_file ~compression filename =
  In_channel.with_file filename ~binary:true ~f:(fun ic ->
    read_single_message_from_channel ~compression ic)


