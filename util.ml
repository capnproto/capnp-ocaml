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


let decode_uint64_le (get : int -> int) : Uint64.t =
  let rec decode acc i =
    if i = 8 then
      acc
    else
      let byte = Uint64.of_int (get i) in
      let shifted_byte = Uint64.shift_left byte (8 * i) in
      decode (Uint64.logor acc shifted_byte) (i + 1)
  in
  decode Uint64.zero 0


let decode_uint32_le (get : int -> int) : Uint32.t =
  let rec decode acc i =
    if i = 4 then
      acc
    else
      let byte = Uint32.of_int (get i) in
      let shifted_byte = Uint32.shift_left byte (8 * i) in
      decode (Uint32.logor acc shifted_byte) (i + 1)
  in
  decode Uint32.zero 0


let decode_uint16_le (get : int -> int) : int =
  let rec decode acc i =
    if i = 2 then
      acc
    else
      let shifted_byte = Int.shift_left (get i) (8 * i) in
      decode (Int.bit_or acc shifted_byte) (i + 1)
  in
  decode 0 0


let decode_int64_le (get : int -> int) : Int64.t =
  let rec decode acc i =
    if i = 8 then
      acc
    else
      let byte = Int64.of_int (get i) in
      let shifted_byte = Int64.shift_left byte (8 * i) in
      decode (Int64.bit_or acc shifted_byte) (i + 1)
  in
  decode Int64.zero 0


let decode_int32_le (get : int -> int) : Int32.t =
  let rec decode acc i =
    if i = 4 then
      acc
    else
      let byte = Int32.of_int_exn (get i) in
      let shifted_byte = Int32.shift_left byte (8 * i) in
      decode (Int32.bit_or acc shifted_byte) (i + 1)
  in
  decode Int32.zero 0


let decode_int16_le (get : int -> int) : int =
  let u16 = decode_uint16_le get in
  decode_signed 16 u16


let decode_int8 (get : int -> int) : int =
  let u8 = get 0 in
  decode_signed 8 u8

