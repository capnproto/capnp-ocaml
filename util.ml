
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


let uint64_le_of_array (bytes : int array) : Uint64.t =
  let () = assert (Array.length bytes = 8) in
  let rec decode acc i =
    if i = Array.length bytes then
      acc
    else
      let byte = Uint64.of_int bytes.(i) in
      let shifted_byte = Uint64.shift_left byte (8 * i) in
      decode (Uint64.logor acc shifted_byte) (i + 1)
  in
  decode Uint64.zero 0

