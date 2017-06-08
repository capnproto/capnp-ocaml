
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


let tag_val_list = 1L

let offset_shift    = 2
let offset_mask     = Int64.shift_left 0x3fffffffL offset_shift
let offset_mask_int = 0x3fffffff lsl offset_shift

let type_shift    = 32
let type_mask     = Int64.shift_left 0x7L type_shift
let type_mask_int = 0x7 lsl type_shift

let count_shift = 35
let count_mask  = Int64.shift_left 0x1fffffffL count_shift

let decode (pointer64 : Int64.t) : t =
  let num_elements =
    let shifted64 = Int64.shift_right_logical pointer64 count_shift in
    (* The count is left-aligned in the field, no mask needed *)
    Int64.to_int shifted64
  in
  (* Int64 arithmetic causes unfortunate GC pressure.  If we're on a 64-bit
     platform, use standard 63-bit ints whenever possible. *)
  if Sys.word_size = 64 then
    let pointer_int = Int64.to_int pointer64 in
    let offset =
      let v = (pointer_int land offset_mask_int) lsr offset_shift in
      Util.decode_signed 30 v
    in
    let element_type =
      let tp = (pointer_int land type_mask_int) lsr type_shift in
      match tp with
      | 0 -> Void
      | 1 -> OneBitValue
      | 2 -> OneByteValue
      | 3 -> TwoByteValue
      | 4 -> FourByteValue
      | 5 -> EightByteValue
      | 6 -> EightBytePointer
      | 7 -> Composite
      | _ -> assert false
    in {
      offset;
      element_type;
      num_elements;
    }
  else
    let offset =
      let masked     = Int64.logand pointer64 offset_mask in
      let offset64   = Int64.shift_right_logical masked offset_shift in
      let offset_int = Int64.to_int offset64 in
      Util.decode_signed 30 offset_int
    in
    let element_type =
      let masked = Int64.logand pointer64 type_mask in
      let tp64   = Int64.shift_right_logical masked type_shift in
      match Int64.to_int tp64 with
      | 0 -> Void
      | 1 -> OneBitValue
      | 2 -> OneByteValue
      | 3 -> TwoByteValue
      | 4 -> FourByteValue
      | 5 -> EightByteValue
      | 6 -> EightBytePointer
      | 7 -> Composite
      | _ -> assert false
    in {
      offset;
      element_type;
      num_elements;
    }


let encode (storage_descr : t) : Int64.t =
  (* Int64 arithmetic causes unfortunate GC pressure.  If we're on a 64-bit
     platform, use standard 63-bit ints whenever possible. *)
  if Sys.word_size = 64 && storage_descr.num_elements <= 0xfffffff then
    let offset = Util.encode_signed 30 storage_descr.offset in
    let tp =
      match storage_descr.element_type with
      | Void             -> 0
      | OneBitValue      -> 1
      | OneByteValue     -> 2
      | TwoByteValue     -> 3
      | FourByteValue    -> 4
      | EightByteValue   -> 5
      | EightBytePointer -> 6
      | Composite        -> 7
    in
    let tag_val_list_int = 1 in
    Int64.of_int
      (tag_val_list_int lor
         (offset lsl offset_shift) lor
         (tp lsl type_shift) lor
         (storage_descr.num_elements lsl count_shift))
  else
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
    Int64.logor (Int64.shift_left offset64 offset_shift) |>
    Int64.logor (Int64.shift_left type64 type_shift) |>
    Int64.logor (Int64.shift_left (Int64.of_int storage_descr.num_elements) count_shift)


