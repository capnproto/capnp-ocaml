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


type compression_t = [ `None | `Packing ]

module WriteContext : sig
  type 'a t

  (** [create ~write ~compression descr] creates a new write context for writing
      data to the specified descriptor.  [compression] specifies the compression
      format, if any.

      The semantics of the [write] function shall mimic that of [Unix.single_write],
      attempting to write a substring from the [buf] and returning the number
      of bytes actually written. *)
  val create : write:('a -> buf:string -> pos:int -> len:int -> int) ->
    compression:compression_t -> 'a -> 'a t

  (** [enqueue_message context message] places the [message] in the outgoing
      queue of the write [context].  No data will be written to the underlying
      descriptor. *)
  val enqueue_message : 'a t -> 'cap Message.BytesMessage.Message.t -> unit

  (** [bytes_remaining context] obtains the number of unwritten bytes currently
      stored in the write [context]. *)
  val bytes_remaining : 'a t -> int

  (** [write context] attempts to write some of the queued data to the
      underlying descriptor associated with the write [context].

      @return the number of bytes actually written *)
  val write : 'a t -> int
end


(** [write_message_to_fd ~compression message fd] writes the specified [message] to
    the given file descriptor, using the specified [compression] method.
    If [restart] is set to [true] (default), then writes failing with error
    code Unix.EINTR will be automatically retried.

    @raise Unix.Unix_error if a write fails *)
val write_message_to_fd : ?restart:bool -> compression:compression_t ->
  'cap Message.BytesMessage.Message.t -> Unix.file_descr -> unit


(** [write_message_to_channel ~compression message chan] writes the specified
    [message] to the given buffered I/O channel, using the specified [compression]
    method. *)
val write_message_to_channel : compression:compression_t ->
  'cap Message.BytesMessage.Message.t -> Pervasives.out_channel -> unit

