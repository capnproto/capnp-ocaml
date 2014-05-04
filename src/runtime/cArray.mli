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

(** The Array module provides a polymorphic container which is similar in spirit
    to the standard OCaml array, and which has similar performance characteristics,
    but which uses a Cap'n Proto list for the underlying storage. *)

type ro = Message.ro
type rw = Message.rw

(** The array type.  The first type parameter represents the read and/or write
    capability, i.e. [ro] or [rw].  The second type parameter is the type of
    elements stored in the array.  The third type parameter describes the
    underlying storage for the array. *)
type ('cap, 'a, 'arr) t = ('cap, 'a, 'arr) InnerArray.t

(** Return the length (number of elements) of the given array. *)
val length : ('cap, 'a, 'arr) t -> int

(** [get a n] returns element number [n] of array [a], indexed from 0.
    @raise Invalid_argument "index out of bounds" if [n] is outside the range
    0 to [length a] - 1. *)
val get : ('cap, 'a, 'arr) t -> int -> 'a

(** [set a n x] places value [x] in element number [n] of array [a], indexed from 0.
    @raise Invalid_argument "index out of bounds" if [n] is outside the range
    0 to [length a] - 1. *)
val set : (rw, 'a, 'arr) t -> int -> 'a -> unit

(** [mem ~equal a x] tests whether element [x] is present in array [a], using the
    provided equality function.  If the equality function is not provided,
    polymorphic compare is used. *)
val mem : ?equal:('a -> 'a -> bool) -> ('cap, 'a, 'arr) t -> 'a -> bool

(** [is_empty a] returns [true] if array [a] has length 0. *)
val is_empty : ('cap, 'a, 'arr) t -> bool

(** [iter a ~f] applies [f] to each element of array [a], in order. *)
val iter : ('cap, 'a, 'arr) t -> f:('a -> unit) -> unit

(** [iteri a ~f] behaves as [iter], but also passes the index of each element
    as an argument to [f]. *)
val iteri : ('cap, 'a, 'arr) t -> f:(int -> 'a -> unit) -> unit

(** [fold a ~init ~f] returns [f (... f (f (f init e1) e2) e3 ...) en], where
    [e1..en] are the elements of [a]. *)
val fold : ('cap, 'a, 'arr) t -> init:'acc -> f:('acc -> 'a -> 'acc) -> 'acc

(** [fold_right a ~f ~init] returns [f e1 (f e2 (... (f en init) ...))]. *)
val fold_right : ('cap, 'a, 'arr) t -> f:('a -> 'acc -> 'acc) -> init:'acc -> 'acc

(** [foldi a ~init f] behaves as [fold], but also passes the index of each
    element as an argument to [f]. *)
val foldi : ('cap, 'a, 'arr) t -> init:'acc -> f:(int -> 'acc -> 'a -> 'acc) -> 'acc

(** [foldi_right a ~f ~init] behaves as [foldi], but also passes the index of
    each element as an argument to [f]. *)
val foldi_right : ('cap, 'a, 'arr) t -> f:(int -> 'a -> 'acc -> 'acc) -> init:'acc -> 'acc

(** [exists a ~f] returns [true] if and only if there exists an element for
    which the provided function evaluates to [true].  This is a short-circuiting
    operation. *)
val exists : ('cap, 'a, 'arr) t -> f:('a -> bool) -> bool

(** [forall a ~f] returns [true] if and only if the provided function evalues to
    [true] for all elements.  This is a short-circuiting operation. *)
val forall : ('cap, 'a, 'arr) t -> f:('a -> bool) -> bool

(** [count a ~f] returns the number of elements for which the provided function
    evaluates to [true]. *)
val count : ('cap, 'a, 'arr) t -> f:('a -> bool) -> int

(** [find a ~f] returns as an [option] the first element for which [f] evaluates
    to [true]. *)
val find : ('cap, 'a, 'arr) t -> f:('a -> bool) -> 'a option

(** find_map a ~f] returns the first evaluation of [f] that returns [Some], and
    returns [None] if there is no such element. *)
val find_map : ('cap, 'a, 'arr) t -> f:('a -> 'b option) -> 'b option

(** [to_list a] returns an OCaml list containing all the elements of [a], in order. *)
val to_list : ('cap, 'a, 'arr) t -> 'a list

(** [to_array a] returns an OCaml array containing all the elements of [a], in order. *)
val to_array : ('cap, 'a, 'arr) t -> 'a array

(** [map_array a ~f] returns [| f e1; f e2; ... f en |], where [e1..en] are the
    elements of [a]. *)
val map_array : ('cap, 'a, 'arr) t -> f:('a -> 'b) -> 'b array

(** [map_list a ~f] returns [f e1; f e2; ... f en], where [e1..en] are the
    elements of [a].  The elements are evaluated in reverse order. *)
val map_list : ('cap, 'a, 'arr) t -> f:('a -> 'b) -> 'b list

