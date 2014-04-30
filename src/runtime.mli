
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

end

module Array : sig
  include ARRAY
end

module InnerArray : sig
  type ('cap, 'a, 'arr) t = {
    length : int;
    get_unsafe : int -> 'a;
    set_unsafe : int -> 'a -> unit;
    storage : 'arr option;
  }

  val length : ('cap, 'a, 'arr) t -> int

  val get : ('cap, 'a, 'arr) t -> int -> 'a

  val set : (rw, 'a, 'arr) t -> int -> 'a -> unit

  val of_outer_array : ('cap, 'a, 'arr) Array.t -> ('cap, 'a, 'arr) t

  val to_outer_array : ('cap, 'a, 'arr) t -> ('cap, 'a, 'arr) Array.t

  val to_storage : ('cap, 'a, 'arr) t -> 'arr option

  val invalid_set_unsafe : int -> 'a -> unit
end


