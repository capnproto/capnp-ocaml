
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
  (** Signed offset in words from end of the pointer to start of the first
      list element. *)
  offset : int;

  (** Type of data stored for each list element. *)
  element_type : element_type_t;

  (** Number of elements in the list.  For Composite list data, this is the number
      of words in the list. *)
  num_elements : int;
}


let tag_val_list = Int64.one

let offset_shift = 2
let offset_mask  = Int64.shift_left (Int64.of_int 0x3fffffff) offset_shift

let type_shift = 32
let type_mask  = Int64.shift_left (Int64.of_int 0x7) type_shift

let count_shift = 35
let count_mask  = Int64.shift_left (Int64.of_int 0x1fffffff) count_shift

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
    let masked  = Int64.bit_and pointer64 count_mask in
    let count64 = Int64.shift_right_logical masked count_shift in
    Int64.to_int_exn count64
  in {
    offset;
    element_type;
    num_elements;
  }


let encode (storage_descr : t) : Int64.t =
  let offset64 = Int64.of_int (Util.encode_signed 30 storage_descr.offset) in
  let type64 =
    let type_id = match storage_descr.element_type with
      | Void             -> 0
      | OneBitValue      -> 1
      | OneByteValue     -> 2
      | TwoByteValue     -> 3
      | FourByteValue    -> 4
      | EightByteValue   -> 5
      | EightBytePointer -> 6
      | Composite        -> 7
    in
    Int64.of_int type_id
  in
  tag_val_list |>
  Int64.bit_or (Int64.shift_left offset64 offset_shift) |>
  Int64.bit_or (Int64.shift_left type64 type_shift) |>
  Int64.bit_or (Int64.shift_left (Int64.of_int storage_descr.num_elements) count_shift)


