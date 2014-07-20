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

(** [FragmentBuffer] provides efficient management of large numbers of string
    fragments.  Reading a large message from a pipe, for example, may lead to
    construction of large numbers of fragments which need to parsed and
    refactored into a list of message segments.  Use of the FragmentBuffer
    helps to avoid the inefficient construction of intermediate strings. *)

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

