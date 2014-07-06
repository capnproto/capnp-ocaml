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


module FramingError = struct
  type t =
    | Incomplete    (** less than a full frame is available *)
    | Unsupported   (** frame header describes a segment count or segment size that
                        is too large for the implementation *)
end


module type DECODER = sig
  (** The type of streams containing framed messages. *)
  type t

  (** [empty ()] returns a new stream containing no data. *)
  val empty : unit -> t

  (** [of_bytes buf] returns a new stream which is filled with the contents
      of the given buffer. *)
  val of_string : string -> t

  (** [add_fragment stream fragment] adds a new fragment to the stream for
      decoding.  Fragments are processed in FIFO order. *)
  val add_fragment : t -> string -> unit

  (** [is_empty stream] determines whether or not the stream contains any
      data which has not yet been fully decoded. *)
  val is_empty : t -> bool

  (** [get_next_frame] attempts to decode the next frame from the stream.
      A successful decode removes the data from the stream and returns the
      frame as a [string list] with one list element for every segment within
      the message. *)
  val get_next_frame : t -> (string list, FramingError.t) Core.Std.Result.t
end

