open Core.Std

type ro
type rw

module type SEGMENT = sig
  type storage_t
  type -'cap t

  val create     : int -> rw t
  val length     : 'cap t -> int
  val get        : 'cap t -> int -> int
  val set        : rw t -> int -> int -> unit
  val readonly   : 'cap t -> ro t
  val of_storage : storage_t -> rw t
  val to_storage : 'cap t -> storage_t
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
  val get         : 'cap t -> int -> int
  val set         : rw t -> int -> int -> unit
  val get_end     : 'cap t -> int
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
    let get          = Storage.get
    let set          = Storage.set
    let readonly s   = s
    let of_storage s = s
    let to_storage s = s
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

    let get slice i =
      if i < 0 || i >= slice.len then
        invalid_arg "Slice.get"
      else
        let segment = get_segment slice in
        Segment.get segment (slice.start + i)

    let set slice i v =
      if i < 0 || i >= slice.len then
        invalid_arg "Slice.get"
      else
        let segment = get_segment slice in
        Segment.set segment (slice.start + i) v

    let get_end slice = slice.start + slice.len
  end
end


exception Invalid_message of string
let invalid_msg s = raise (Invalid_message s)


