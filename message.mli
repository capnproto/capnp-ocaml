type ro
type rw

module type SEGMENT = sig
  type storage_t
  type -'cap t

  val create     : int -> rw t
  val length     : 'cap t -> int
  val readonly   : 'cap t -> ro t
  val of_storage : storage_t -> rw t
  val to_storage : 'cap t -> storage_t

  val get_uint8  : 'cap t -> int -> int
  val get_uint16 : 'cap t -> int -> int
  val get_uint32 : 'cap t -> int -> Uint32.t
  val get_uint64 : 'cap t -> int -> Uint64.t
  val get_int8   : 'cap t -> int -> int
  val get_int16  : 'cap t -> int -> int
  val get_int32  : 'cap t -> int -> Int32.t
  val get_int64  : 'cap t -> int -> Int64.t
  val set_uint8  : rw t -> int -> int -> unit
  val set_uint16 : rw t -> int -> int -> unit
  val set_uint32 : rw t -> int -> Uint32.t -> unit
  val set_uint64 : rw t -> int -> Uint64.t -> unit
  val set_int8   : rw t -> int -> int -> unit
  val set_int16  : rw t -> int -> int -> unit
  val set_int32  : rw t -> int -> Int32.t -> unit
  val set_int64  : rw t -> int -> Int64.t -> unit
end

module type MESSAGE = sig
  type storage_t
  type -'cap segment_t
  type -'cap t

  val create       : int -> rw t
  val num_segments : 'cap t -> int
  val get_segment  : 'cap t -> int -> 'cap segment_t
  val add_segment  : rw t -> int -> unit
  val readonly     : 'cap t -> ro t
  val of_storage   : storage_t list -> rw t
  val to_storage   : 'cap t -> storage_t list
end

module type SLICE = sig
  type -'cap segment_t
  type -'cap message_t

  type 'cap t = {
    msg        : 'cap message_t;
    segment_id : int;
    start      : int;
    len        : int;
  }

  val get_segment : 'cap t -> 'cap segment_t
  val get_end     : 'cap t -> int

  val get_uint8  : 'cap t -> int -> int
  val get_uint16 : 'cap t -> int -> int
  val get_uint32 : 'cap t -> int -> Uint32.t
  val get_uint64 : 'cap t -> int -> Uint64.t
  val get_int8   : 'cap t -> int -> int
  val get_int16  : 'cap t -> int -> int
  val get_int32  : 'cap t -> int -> Int32.t
  val get_int64  : 'cap t -> int -> Int64.t
  val set_uint8  : rw t -> int -> int -> unit
  val set_uint16 : rw t -> int -> int -> unit
  val set_uint32 : rw t -> int -> Uint32.t -> unit
  val set_uint64 : rw t -> int -> Uint64.t -> unit
  val set_int8   : rw t -> int -> int -> unit
  val set_int16  : rw t -> int -> int -> unit
  val set_int32  : rw t -> int -> Int32.t -> unit
  val set_int64  : rw t -> int -> Int64.t -> unit
end

module type S = sig
  module Segment : sig
    include SEGMENT
  end

  module Message : sig
    include MESSAGE with type 'a segment_t := 'a Segment.t
  end

  module Slice : sig
    include SLICE with type 'a segment_t := 'a Segment.t and type 'a message_t := 'a Message.t
  end
end

module Make (Storage : MessageStorage.S) :
  (S with type Segment.storage_t = Storage.t and type Message.storage_t = Storage.t)

exception Invalid_message of string
val invalid_msg : string -> 'a

