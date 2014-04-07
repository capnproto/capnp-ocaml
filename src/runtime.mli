
module Array : sig
  type ('a, 'arr) t

  val length : ('a, 'arr) t -> int

  val get : ('a, 'arr) t -> int -> 'a

  val make : length:('arr -> int) -> get:('arr -> int -> 'a) -> 'arr -> ('a, 'arr) t

  val make_default : unit -> ('a, 'arr) t
end


module BArray : sig
  type ('a, 'arr) t

  val length : ('a, 'arr) t -> int

  val get : ('a, 'arr) t -> int -> 'a

  val set : ('a, 'arr) t -> int -> 'a -> unit

  val make :
    length:('arr -> int) ->
    get:('arr -> int -> 'a) ->
    set:('arr -> int -> 'a -> unit) ->
    'arr -> ('a, 'arr) t
end
