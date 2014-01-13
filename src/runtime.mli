
module Array : sig
  type ('a, 'arr) t

  val length : ('a, 'arr) t -> int

  val get : ('a, 'arr) t -> int -> 'a

  val make : length:('arr -> int) -> get:('arr -> int -> 'a) -> 'arr -> ('a, 'arr) t

  val make_default : unit -> ('a, 'arr) t
end

