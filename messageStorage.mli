

module type S = sig
  type t

  val create : int -> t
  val length : t -> int
  val get    : t -> int -> int
  val set    : t -> int -> int -> unit
end

