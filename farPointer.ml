type landing_pad_t =
  | NormalPointer
  | TaggedFarPointer

type t = {
  landing_pad : landing_pad_t;
  offset      : int;
  segment_id  : int
}

let landing_pad_type_shift = 61
let landing_pad_type_mask  = Uint64.shift_left Uint64.one landing_pad_type_shift

let offset_shift = 32
let offset_mask  = Uint64.shift_left (Uint64.of_int 0x1fffffff) offset_shift

let segment_mask = Uint64.sub (Uint64.shift_left Uint64.one 32) Uint64.one

let decode (pointer64 : Uint64.t) : t =
  let landing_pad =
    let masked = Uint64.logand pointer64 landing_pad_type_mask in
    if Uint64.compare masked Uint64.zero = 0 then
      NormalPointer
    else
      TaggedFarPointer
  in
  let offset =
    let masked = Uint64.logand pointer64 offset_mask in
    let offset64 = Uint64.shift_right masked offset_shift in
    Uint64.to_int offset64
  in
  let segment_id =
    let max64 = Uint64.of_int max_int in
    let id64  = Uint64.logand pointer64 segment_mask in
    if Uint64.compare id64 max64 > 0 then
      Message.invalid_msg "far pointer contains segment ID larger than OCaml max_int"
    else
      Uint64.to_int id64
  in {
    landing_pad;
    offset;
    segment_id;
  }

