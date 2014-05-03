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

type ro = Message.ro
type rw = Message.rw

module type ARRAY = sig
  type ('cap, 'a, 'arr) t

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

  (** [to_list a] returns a list containing all the elements of [a], in order. *)
  val to_list : ('cap, 'a, 'arr) t -> 'a list

  (** [to_array a] returns an array containing all the elements of [a], in order. *)
  val to_array : ('cap, 'a, 'arr) t -> 'a array

  (** [map_array a ~f] returns [| f e1; f e2; ... f en |], where [e1..en] are the
      elements of [a]. *)
  val map_array : ('cap, 'a, 'arr) t -> f:('a -> 'b) -> 'b array

  (** [map_list a ~f] returns [f e1; f e2; ... f en], where [e1..en] are the
      elements of [a].  The elements are evaluated in reverse order. *)
  val map_list : ('cap, 'a, 'arr) t -> f:('a -> 'b) -> 'b list
end

module InnerArray = struct
  type ('cap, 'a, 'arr) t = {
    length : int;
    get_unsafe : int -> 'a;
    set_unsafe : int -> 'a -> unit;
    storage : 'arr option;
  }

  let length x = x.length

  let get x i =
    if i < 0 || i >= x.length then
      invalid_arg "index out of bounds"
    else
      x.get_unsafe i

  let set x i v =
    if i < 0 || i >= x.length then
      invalid_arg "index out of bounds"
    else
      x.set_unsafe i v

  let of_outer_array x = x

  let to_outer_array x = x

  let to_storage x = x.storage

  let invalid_set_unsafe i v = assert false
end


module Array = struct
  type ('cap, 'a, 'arr) t = ('cap, 'a, 'arr) InnerArray.t

  let length = InnerArray.length
  let get    = InnerArray.get
  let set    = InnerArray.set

  let foldi a ~init ~f =
    let len = length a in
    let () = assert (len >= 0) in
    let rec loop acc i =
      if i = len then
        acc
      else
        let x = get a i in
        loop (f i acc x) (i + 1)
    in
    loop init 0

  let foldi_right a ~f ~init =
    let rec loop acc i =
      if i < 0 then
        acc
      else
        let x = get a i in
        loop (f i x acc) (i - 1)
    in
    let len = length a in
    loop init (len - 1)

  let fold a ~init ~f =
    foldi a ~init ~f:(fun i acc x -> f acc x)

  let fold_right a ~f ~init =
    foldi_right a ~f:(fun i x acc -> f x acc) ~init

  let mem ?equal a x =
    let len = length a in
    let () = assert (len >= 0) in
    let real_equal =
      match equal with
      | Some user_equal ->
          user_equal
      | None ->
          Pervasives.(=)
    in
    let rec loop i =
      if i = len then
        false
      else
        if real_equal x (get a i) then
          true
        else
          loop (i + 1)
    in
    loop 0

  let is_empty a = (length a) = 0

  let iter a ~f =
    fold a ~init:() ~f:(fun () x -> f x)

  let iteri a ~f =
    foldi a ~init:() ~f:(fun i () x -> f i x)

  let exists a ~f =
    let len = length a in
    let () = assert (len >= 0) in
    let rec loop i =
      if i = len then
        false
      else
        if f (get a i) then
          true
        else
          loop (i + 1)
    in
    loop 0

  let forall a ~f =
    let len = length a in
    let () = assert (len >= 0) in
    let rec loop i =
      if i = len then
        true
      else
        if f (get a i) then
          loop (i + 1)
        else
          false
    in
    loop 0

  let count a ~f =
    let fold_f acc x = if f x then acc + 1 else acc in
    fold a ~init:0 ~f:fold_f

  let find a ~f =
    let len = length a in
    let () = assert (len >= 0) in
    let rec loop i =
      if i = len then
        None
      else
        let x = get a i in
        if f x then
          Some x
        else
          loop (i + 1)
    in
    loop 0

  let find_map a ~f =
    let len = length a in
    let () = assert (len >= 0) in
    let rec loop i =
      if i = len then
        None
      else
        let r = f (get a i) in
        match r with
        | Some _ -> r
        | None   -> loop (i + 1)
    in
    loop 0

  let to_list a =
    List.init (length a) ~f:(fun i -> get a i)

  let to_array a =
    Array.init (length a) (fun i -> get a i)

  let map_array a ~f =
    Array.init (length a) (fun i -> f (get a i))

  let map_list a ~f =
    List.init (length a) ~f:(fun i -> f (get a i))

end


