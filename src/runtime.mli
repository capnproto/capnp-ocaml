
type ro = Message.ro
type rw = Message.rw

module type ARRAY = sig
  type ('cap, 'a, 'arr) t

  val length : ('cap, 'a, 'arr) t -> int

  val get : ('cap, 'a, 'arr) t -> int -> 'a

  val set : (rw, 'a, 'arr) t -> int -> 'a -> unit
end

module Array : sig
  include ARRAY
end

module InnerArray : sig
  (* If module ListStorage were defined elsewhere, then InnerArray
     could reference it directly and we could drop the third argument.

     But this would fail if we wanted to construct an [ro] array based on
     rw ListStorage.  And that's a pretty reasonable thing to do.

     I guess in that case we could use a constructor function
     [readonly : 'cap ListStorage.t -> ro ListStorage.t]? *)
  type ('cap, 'a, 'arr) t = {
    length : unit -> int;    (* FIXME: should be just [int] *)
    get_unsafe : int -> 'a;
    set_unsafe : int -> 'a -> unit;
    storage : 'arr option;
  }

  include ARRAY with type ('a, 'b, 'c) t := ('a, 'b, 'c) t

  val of_outer_array : ('cap, 'a, 'arr) Array.t -> ('cap, 'a, 'arr) t

  val to_outer_array : ('cap, 'a, 'arr) t -> ('cap, 'a, 'arr) Array.t

  val to_storage : ('cap, 'a, 'arr) t -> 'arr option

  val invalid_set_unsafe : int -> 'a -> unit
end
  

