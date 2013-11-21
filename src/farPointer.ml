module Int64 = Core.Core_int64

type landing_pad_t =
  | NormalPointer
  | TaggedFarPointer

type t = {
  landing_pad : landing_pad_t;
  offset      : int;
  segment_id  : int
}

let landing_pad_type_shift = 2
let landing_pad_type_mask  = Int64.shift_left Int64.one landing_pad_type_shift

let offset_shift = 3
let offset_mask  = Int64.shift_left (Int64.of_int 0x1fffffff) offset_shift

let segment_shift = 32
let segment_mask  =
  Int64.shift_left (Int64.(-) (Int64.shift_left Int64.one 32) Int64.one) segment_shift

let decode (pointer64 : Int64.t) : t =
  let landing_pad =
    let masked = Int64.bit_and pointer64 landing_pad_type_mask in
    if Int64.compare masked Int64.zero = 0 then
      NormalPointer
    else
      TaggedFarPointer
  in
  let offset =
    let masked = Int64.bit_and pointer64 offset_mask in
    let offset64 = Int64.shift_right_logical masked offset_shift in
    Int64.to_int_exn offset64
  in
  let segment_id =
    let max64  = Int64.of_int max_int in
    let masked = Int64.bit_and pointer64 segment_mask in
    let id64   = Int64.shift_right_logical masked segment_shift in
    if Int64.compare id64 max64 > 0 then
      Message.invalid_msg "far pointer contains segment ID larger than OCaml max_int"
    else
      Int64.to_int_exn id64
  in {
    landing_pad;
    offset;
    segment_id;
  }

