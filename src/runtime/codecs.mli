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


module FramingError : sig
  type t = CodecsSig.FramingError.t =
    | Incomplete    (** less than a full frame is available *)
    | Unsupported   (** frame header describes a segment count or segment size that
                        is too large for the implementation *)
end

module FramedStream : sig
  include CodecsSig.DECODER

  (** A streaming decoder for the Cap'n Proto "standard serialization"
      (non-packed message segments prefixed by framing information). *)
end


module PackedStream : sig
  include CodecsSig.DECODER

  (** A streaming decoder for "packed" messages (i.e. messages encoded
      using "standard serialization" and then compressed with the
      standard packing method). *)
end


(** [serialize_fold message ~init f] generates an ordered sequence of
    bytes fragments corresponding to a Cap'n Proto framed message
    using the standard serialization.  The return value is the result
    of folding [f] across the resulting sequence of fragments. *)
val serialize_fold : Bytes.t list -> init:'acc -> f:('acc -> Bytes.t -> 'acc) -> 'acc

(** [serialize_iter message ~f] generates an ordered sequence of bytes
    fragments corresponding to a Cap'n Proto framed message using the standard
    serialization.  [f] is applied to each fragment in turn. *)
val serialize_iter : Bytes.t list -> f:(Bytes.t -> unit) -> unit

(** [serialize message] constructs a buffer containing the [message] segments
    with a standard serialization framing header. *)
val serialize : Bytes.t list -> string

(** [pack_fold message ~init f] generates an ordered sequence of
    bytes fragments corresponding to a packed Cap'n Proto message.
    The return value is the result of folding [f] across the resulting
    sequence of fragments. *)
val pack_fold : Bytes.t list -> init:'acc -> f:('acc -> Bytes.t -> 'acc) -> 'acc

(** [pack_iter message ~f] generates an ordered sequence of bytes
    fragments corresponding to a packed Cap'n Proto message.  [f] is applied
    to each fragment in turn. *)
val pack_iter : Bytes.t list -> f:(Bytes.t -> unit) -> unit

(** [pack message] constructs a buffer containing a packed Cap'n Proto message. *)
val pack : Bytes.t list -> string

