
open Core.Std

(* Decode [num] as a signed integer of width [n] bits, using two's complement
 * representation of negative numbers. *)
let decode_signed n num =
  let power_of_two = 1 lsl (n - 1) in
  let is_signed = (num land power_of_two) <> 0 in
  if is_signed then
    (num land (power_of_two - 1)) - power_of_two
  else
    num


let ceil_int num denom = (num + denom - 1) / denom


