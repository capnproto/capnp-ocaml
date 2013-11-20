module Int64 = Core.Core_int64;;

type element_type_t =
  | Void
  | OneBitValue
  | OneByteValue
  | TwoByteValue
  | FourByteValue
  | EightByteValue
  | EightBytePointer
  | Composite

type t = {
  offset       : int;
  element_type : element_type_t;
  num_elements : int;
}

let offset_shift = 32
let offset_mask  = Int64.shift_left (Int64.of_int 0x3fffffff) offset_shift

let type_shift = 29
let type_mask  = Int64.shift_left (Int64.of_int 0x7) type_shift

let count_mask = Int64.of_int 0x1fffffff

let decode (pointer64 : Int64.t) : t =
  let offset =
    let masked     = Int64.bit_and pointer64 offset_mask in
    let offset64   = Int64.shift_right_logical masked offset_shift in
    let offset_int = Int64.to_int_exn offset64 in
    Util.decode_signed 30 offset_int
  in
  let element_type =
    let masked = Int64.bit_and pointer64 type_mask in
    let tp64   = Int64.shift_right_logical masked type_shift in
    match Int64.to_int_exn tp64 with
    | 0 -> Void
    | 1 -> OneBitValue
    | 2 -> OneByteValue
    | 3 -> TwoByteValue
    | 4 -> FourByteValue
    | 5 -> EightByteValue
    | 6 -> EightBytePointer
    | 7 -> Composite
    | _ -> assert false
  in
  let num_elements =
    let masked = Int64.bit_and pointer64 count_mask in
    Int64.to_int_exn masked
  in {
    offset;
    element_type;
    num_elements;
  }

