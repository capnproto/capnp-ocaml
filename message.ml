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

module Make (Storage : MessageStorage.S) = struct

  module Segment = struct
    type -'cap t = Storage.t

    let create = Storage.create
    let length = Storage.length
    let get    = Storage.get
    let set    = Storage.set
    let readonly s = s
  end

  module Message = struct
    type -'cap segment_t = Storage.t
    type -'cap t = Storage.t Res.Array.t

    let create size =
      let segment = Storage.create size in
      Res.Array.create 1 segment

    let num_segments = Res.Array.length

    let get_segment  = Res.Array.get

    let add_segment m size =
      let new_segment = Storage.create size in
      Res.Array.add_one m new_segment
    let readonly m = m
  end
end


