type ro
type rw

module type SEGMENT = sig
  type -'cap t

  val create   : int -> rw t
  val length   : 'cap t -> int
  val get      : 'cap t -> int -> int
  val set      : rw t -> int -> int -> unit
  val readonly : 'cap t -> ro t
end

module type MESSAGE = sig
  type -'cap segment_t
  type -'cap t

  val create       : int -> rw t
  val num_segments : 'cap t -> int
  val get_segment  : 'cap t -> int -> 'cap segment_t
  val add_segment  : rw t -> int -> unit
  val readonly     : 'cap t -> ro t
end

module type S = sig
  module Segment : sig
    include SEGMENT
  end

  module Message : sig
    include MESSAGE with type 'a segment_t = 'a Segment.t
  end
end

module Make (Storage : MessageStorage.S) : S


