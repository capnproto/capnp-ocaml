

module type S = sig
  type t

  val create : int -> t
  val length : t -> int

  val get_uint8  : t -> int -> int
  val get_uint16 : t -> int -> int
  val get_uint32 : t -> int -> Uint32.t
  val get_uint64 : t -> int -> Uint64.t

  val get_int8   : t -> int -> int
  val get_int16  : t -> int -> int
  val get_int32  : t -> int -> Int32.t
  val get_int64  : t -> int -> Int64.t

  val set_uint8  : t -> int -> int -> unit
  val set_uint16 : t -> int -> int -> unit
  val set_uint32 : t -> int -> Uint32.t -> unit
  val set_uint64 : t -> int -> Uint64.t -> unit

  val set_int8   : t -> int -> int -> unit
  val set_int16  : t -> int -> int -> unit
  val set_int32  : t -> int -> Int32.t -> unit
  val set_int64  : t -> int -> Int64.t -> unit
end

