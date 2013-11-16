open Core.Std

type t = string

let length = String.length

let get s i = Char.to_int s.[i]

let set s i v =
  try
    s.[i] <- (Char.of_int_exn v)
  with Failure _ ->
    invalid_arg "StrStorage.set"

let create = String.create

