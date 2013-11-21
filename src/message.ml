open Core.Std

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

module Make (Storage : MessageStorage.S) = struct

  module Segment = struct
    type storage_t = Storage.t
    type -'cap t = Storage.t

    let create       = Storage.create
    let length       = Storage.length
    let readonly s   = s
    let of_storage s = s
    let to_storage s = s

    let get_uint8  = Storage.get_uint8
    let get_uint16 = Storage.get_uint16
    let get_uint32 = Storage.get_uint32
    let get_uint64 = Storage.get_uint64
    let get_int8   = Storage.get_int8
    let get_int16  = Storage.get_int16
    let get_int32  = Storage.get_int32
    let get_int64  = Storage.get_int64
    let set_uint8  = Storage.set_uint8
    let set_uint16 = Storage.set_uint16
    let set_uint32 = Storage.set_uint32
    let set_uint64 = Storage.set_uint64
    let set_int8   = Storage.set_int8
    let set_int16  = Storage.set_int16
    let set_int32  = Storage.set_int32
    let set_int64  = Storage.set_int64
  end

  module Message = struct
    type storage_t = Storage.t
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

    let of_storage ms =
      let arr = Res.Array.empty () in
      let () = List.iter ms ~f:(fun x -> Res.Array.add_one arr x) in
      arr

    let to_storage m = Res.Array.fold_right (fun x acc -> x :: acc) m []
  end

  module Slice = struct
    type -'cap segment_t = Storage.t
    type -'cap message_t = Storage.t Res.Array.t

    type 'cap t = {
      msg        : 'cap message_t;
      segment_id : int;
      start      : int;
      len        : int;
    }

    let get_segment slice = Message.get_segment slice.msg slice.segment_id

    let get_end slice = slice.start + slice.len

    let get_uint8 slice i =
      if i < 0 || i > slice.len - 1 then
        invalid_arg "Slice.get_uint8"
      else
        let segment = get_segment slice in
        Segment.get_uint8 segment (slice.start + i)

    let get_uint16 slice i =
      if i < 0 || i > slice.len - 2 then
        invalid_arg "Slice.get_uint16"
      else
        let segment = get_segment slice in
        Segment.get_uint16 segment (slice.start + i)

    let get_uint32 slice i =
      if i < 0 || i > slice.len - 4 then
        invalid_arg "Slice.get_uint32"
      else
        let segment = get_segment slice in
        Segment.get_uint32 segment (slice.start + i)

    let get_uint64 slice i =
      if i < 0 || i > slice.len - 8 then
        invalid_arg "Slice.get_uint64"
      else
        let segment = get_segment slice in
        Segment.get_uint64 segment (slice.start + i)

    let get_int8 slice i =
      if i < 0 || i > slice.len - 1 then
        invalid_arg "Slice.get_int8"
      else
        let segment = get_segment slice in
        Segment.get_int8 segment (slice.start + i)

    let get_int16 slice i =
      if i < 0 || i > slice.len - 2 then
        invalid_arg "Slice.get_int16"
      else
        let segment = get_segment slice in
        Segment.get_int16 segment (slice.start + i)

    let get_int32 slice i =
      if i < 0 || i > slice.len - 4 then
        invalid_arg "Slice.get_int32"
      else
        let segment = get_segment slice in
        Segment.get_int32 segment (slice.start + i)

    let get_int64 slice i =
      if i < 0 || i > slice.len - 8 then
        invalid_arg "Slice.get_int64"
      else
        let segment = get_segment slice in
        Segment.get_int64 segment (slice.start + i)

    let set_uint8 slice i v =
      if i < 0 || i > slice.len - 1 then
        invalid_arg "Slice.set_uint8"
      else
        let segment = get_segment slice in
        Segment.set_uint8 segment (slice.start + i) v

    let set_uint16 slice i v =
      if i < 0 || i > slice.len - 2 then
        invalid_arg "Slice.set_uint16"
      else
        let segment = get_segment slice in
        Segment.set_uint16 segment (slice.start + i) v

    let set_uint32 slice i v =
      if i < 0 || i > slice.len - 4 then
        invalid_arg "Slice.set_uint32"
      else
        let segment = get_segment slice in
        Segment.set_uint32 segment (slice.start + i) v

    let set_uint64 slice i v =
      if i < 0 || i > slice.len - 8 then
        invalid_arg "Slice.set_uint64"
      else
        let segment = get_segment slice in
        Segment.set_uint64 segment (slice.start + i) v

    let set_int8 slice i v =
      if i < 0 || i > slice.len - 1 then
        invalid_arg "Slice.set_int8"
      else
        let segment = get_segment slice in
        Segment.set_int8 segment (slice.start + i) v

    let set_int16 slice i v =
      if i < 0 || i > slice.len - 2 then
        invalid_arg "Slice.set_int16"
      else
        let segment = get_segment slice in
        Segment.set_int16 segment (slice.start + i) v

    let set_int32 slice i v =
      if i < 0 || i > slice.len - 4 then
        invalid_arg "Slice.set_int32"
      else
        let segment = get_segment slice in
        Segment.set_int32 segment (slice.start + i) v

    let set_int64 slice i v =
      if i < 0 || i > slice.len - 8 then
        invalid_arg "Slice.set_int64"
      else
        let segment = get_segment slice in
        Segment.set_int64 segment (slice.start + i) v
  end
end


exception Invalid_message of string
let invalid_msg s = raise (Invalid_message s)


