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
    match context.comp with
    | `None ->
        Codecs.serialize_iter message ~f:(fun bytes_buf ->
          let buf_copy = Bytes.to_string bytes_buf in
          Dequeue.enqueue_back context.fragments buf_copy;
          context.fragments_size <- context.fragments_size + (String.length buf_copy))
    | `Packing ->
        Codecs.pack_iter message ~f:(fun buf ->
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
end


module ReadContext = struct
  type ('a, 'stream) t = {
    (** File descriptor we're writing to *)
    fd : 'a;

    (** Stream format *)
    stream : 'stream;

    (** Function for reading from the descriptor *)
    read : 'a -> buf:Bytes.t -> pos:int -> len:int -> int;

    (** Persistent read buffer *)
    read_buf : Bytes.t;

    (** Function for getting the available byte count *)
    bytes_available : 'stream -> int;

    (** Function for adding some data to the stream *)
    add_fragment : 'stream -> string -> unit;

    (** Function for getting a frame from the stream *)
    get_next_frame : 'stream -> (string list, Codecs.FramingError.t) Result.t;
  }

  let create_standard ~read fd = {
    fd;
    stream = Codecs.FramedStream.empty ();
    read;
    read_buf = Bytes.create (64 * 1024);    (* Size of ocaml internal Unix buffer *)
    bytes_available = Codecs.FramedStream.bytes_available;
    add_fragment = Codecs.FramedStream.add_fragment;
    get_next_frame = Codecs.FramedStream.get_next_frame;
  }

  let create_packed ~read fd = {
    fd;
    stream = Codecs.PackedStream.empty ();
    read;
    read_buf = Bytes.create (64 * 1024);    (* Size of ocaml internal Unix buffer *)
    bytes_available = Codecs.PackedStream.bytes_available;
    add_fragment = Codecs.PackedStream.add_fragment;
    get_next_frame = Codecs.PackedStream.get_next_frame;
  }

  let dequeue_message context =
    match context.get_next_frame context.stream with
    | Result.Ok segments ->
        Some (Message.BytesMessage.Message.of_storage segments)
    | Result.Error Codecs.FramingError.Incomplete ->
        None
    | Result.Error Codecs.FramingError.Unsupported ->
        raise Unsupported_message_frame

  let bytes_available context = context.bytes_available context.stream

  let read context =
    let bytes_read = context.read context.fd ~buf:context.read_buf
        ~pos:0 ~len:(Bytes.length context.read_buf)
    in
    if bytes_read > 0 then
      let substr = String.sub context.read_buf ~pos:0 ~len:bytes_read in
      let () = context.add_fragment context.stream substr in
      bytes_read
    else
      bytes_read
end


let create_write_context_for_fd ?(restart = true) ~compression fd =
  let unix_write fd' ~buf ~pos ~len =
    Unix.single_write ~restart ~buf ~pos ~len fd'
  in
  WriteContext.create ~write:unix_write ~compression fd


let create_write_context_for_channel ~compression chan =
  let chan_write chan' ~buf ~pos ~len =
    let () = Out_channel.output chan' ~buf ~pos ~len in
    len
  in
  WriteContext.create ~write:chan_write ~compression chan


let create_standard_read_context_for_fd ?(restart = true) fd =
  let unix_read fd' ~buf ~pos ~len =
    Unix.read fd' ~restart ~buf ~pos ~len
  in
  ReadContext.create_standard ~read:unix_read fd


let create_packed_read_context_for_fd ?(restart = true) fd =
  let unix_read fd' ~buf ~pos ~len =
    Unix.read fd' ~restart ~buf ~pos ~len
  in
  ReadContext.create_packed ~read:unix_read fd


let create_standard_read_context_for_channel chan =
  ReadContext.create_standard ~read:In_channel.input chan

let create_packed_read_context_for_channel chan =
  ReadContext.create_packed ~read:In_channel.input chan


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
        let (_ : Unix.Select_fds.t) = Unix.select ~restart
            ~read:[] ~write:[fd] ~except:[fd] ~timeout:`Never ()
        in
        ()
  done


let write_message_to_channel ~compression message chan =
  let context = create_write_context_for_channel ~compression chan in
  let () = WriteContext.enqueue_message context message in
  while WriteContext.bytes_remaining context > 0 do
    let (_ : int) = WriteContext.write context in
    ()
  done


let read_single_message_fd ~restart context fd =
  let rec read_loop () =
    try
      ReadContext.read context
    with
    | Unix.Unix_error (Unix.EAGAIN, _, _)
    | Unix.Unix_error (Unix.EWOULDBLOCK, _, _) ->
        (* Avoid burning CPU time looping on EAGAIN *)
        let (_ : Unix.Select_fds.t) = Unix.select ~restart
            ~read:[] ~write:[fd] ~except:[fd] ~timeout:`Never ()
        in
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


let read_single_message_from_fd ?(restart = true) ~compression fd =
  match compression with
  | `None ->
      let context = create_standard_read_context_for_fd ~restart fd in
      read_single_message_fd ~restart context fd
  | `Packing ->
      let context = create_packed_read_context_for_fd ~restart fd in
      read_single_message_fd ~restart context fd


let read_single_message_from_channel ~compression chan =
  let rec loop context =
    let bytes_read = ReadContext.read context in
    if bytes_read = 0 then
      None
    else
      match ReadContext.dequeue_message context with
      | Some message ->
          Some message
      | None ->
          loop context
  in
  match compression with
  | `None ->
      let context = create_standard_read_context_for_channel chan in
      loop context
  | `Packing ->
      let context = create_packed_read_context_for_channel chan in
      loop context


