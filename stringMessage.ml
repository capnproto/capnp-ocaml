
module StrStorage : MessageStorage.S = struct
  type t = string

  let length = String.length

  let get s i = int_of_char s.[i]

  let set s i v =
    try
      s.[i] <- (char_of_int v)
    with Invalid_argument _ ->
      invalid_arg "StrStorage.set"

  let create = String.create
end

