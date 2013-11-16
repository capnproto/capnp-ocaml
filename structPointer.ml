type t = {
  offset        : int;
  data_size     : int;
  pointers_size : int;
}

let offset_shift = 32
let offset_mask  = Uint64.shift_left (Uint64.of_int 0x3fffffff) offset_shift

let data_size_shift = 16
let data_size_mask  = Uint64.shift_left (Uint64.of_int 0xffff) data_size_shift

let pointers_size_mask = Uint64.of_int 0xffff

let decode (pointer64 : Uint64.t) : t =
  let offset =
    let masked     = Uint64.logand pointer64 offset_mask in
    let offset64   = Uint64.shift_right masked offset_shift in
    let offset_int = Uint64.to_int offset64 in
    Util.decode_signed 30 offset_int
  in
  let data_size =
    let masked = Uint64.logand pointer64 data_size_mask in
    let size64 = Uint64.shift_right masked data_size_shift in
    Uint64.to_int size64
  in
  let pointers_size =
    let masked = Uint64.logand pointer64 pointers_size_mask in
    Uint64.to_int masked
  in {
    offset;
    data_size;
    pointers_size;
  }
