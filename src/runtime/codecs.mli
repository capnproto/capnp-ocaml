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


module FramingError : sig
  type t =
    | Incomplete    (** less than a full frame is available *)
    | Unsupported   (** frame header describes a segment count or segment size that
                        is too large for the implementation *)
end


module FramedStream : sig
  (** The type of streams containing framed messages. *)
  type t

  (** [empty compression] returns a new stream for decoding data stored
      with the given [compression] method.  The stream initially contains
      no data. *)
  val empty : compression_t -> t

  (** [of_string ~compression buf] returns a new stream which is filled with
      the contents of the given buffer, where the buffer contains data
      compressed with the specified [compression] method. *)
  val of_string : compression:compression_t -> string -> t

  (** [add_fragment stream fragment] adds a new fragment to the stream for
      decoding.  Fragments are processed in FIFO order. *)
  val add_fragment : t -> string -> unit

  (** [bytes_available stream] obtains the number of bytes in the stream
      which have not yet been fully decoded. *)
  val bytes_available : t -> int

  (** [is_empty stream] determines whether or not the stream contains any
      data which has not yet been fully decoded. *)
  val is_empty : t -> bool

  (** [get_next_frame] attempts to decode the next frame from the stream.
      A successful decode removes the data from the stream and returns the
      frame as a [string list] with one list element for every segment within
      the message. *)
  val get_next_frame : t ->
    (Message.rw Message.BytesMessage.Message.t, FramingError.t) Core.Std.Result.t
end


(** [serialize_fold message ~init f] generates an ordered sequence of
    bytes fragments corresponding to a Cap'n Proto framed message
    using the standard serialization.  The return value is the result
    of folding [f] across the resulting sequence of fragments. *)
val serialize_fold : 'cap Message.BytesMessage.Message.t ->
  init:'acc -> f:('acc -> Bytes.t -> 'acc) -> 'acc

(** [serialize_iter message ~f] generates an ordered sequence of bytes
    fragments corresponding to a Cap'n Proto framed message using the standard
    serialization.  [f] is applied to each fragment in turn. *)
val serialize_iter : 'cap Message.BytesMessage.Message.t ->
  f:(Bytes.t -> unit) -> unit

(** [serialize message] constructs a buffer containing the [message] segments
    with a standard serialization framing header. *)
val serialize : 'cap Message.BytesMessage.Message.t -> string

(** [pack_fold message ~init f] generates an ordered sequence of
    bytes fragments corresponding to a packed Cap'n Proto message.
    The return value is the result of folding [f] across the resulting
    sequence of fragments. *)
val pack_fold : 'cap Message.BytesMessage.Message.t -> init:'acc ->
  f:('acc -> string -> 'acc) -> 'acc

(** [pack_iter message ~f] generates an ordered sequence of string fragments
    corresponding to a packed Cap'n Proto message.  [f] is applied
    to each fragment in turn. *)
val pack_iter : 'cap Message.BytesMessage.Message.t -> f:(string -> unit) -> unit

(** [pack message] constructs a buffer containing a packed Cap'n Proto message. *)
val pack : 'cap Message.BytesMessage.Message.t -> string

