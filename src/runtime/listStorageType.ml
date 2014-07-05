
let sizeof_uint64 = 8

type t =
  (** list(void), no storage required *)
  | Empty

  (** list(bool), tightly packed bits *)
  | Bit

  (** either primitive values or a data-only struct *)
  | Bytes1
  | Bytes2
  | Bytes4
  | Bytes8

  (** either a pointer to an external object, or a pointer-only struct *)
  | Pointer

  (** typical struct; parameters are per-element word size for data section
      and pointers section, respectively *)
  | Composite of int * int

let get_byte_count storage_type =
  match storage_type with
  | Empty   -> 0
  | Bit     -> assert false
  | Bytes1  -> 1
  | Bytes2  -> 2
  | Bytes4  -> 4
  | Bytes8  -> 8
  | Pointer -> 8
  | Composite (data_words, pointer_words) ->
      (data_words + pointer_words) * sizeof_uint64

let to_string storage_type =
  match storage_type with
  | Empty   -> "ListStorageType.Empty"
  | Bit     -> "ListStorageType.Bit"
  | Bytes1  -> "ListStorageType.Bytes1"
  | Bytes2  -> "ListStorageType.Bytes2"
  | Bytes4  -> "ListStorageType.Bytes4"
  | Bytes8  -> "ListStorageType.Bytes8"
  | Pointer -> "ListStorageType.Pointer"
  | Composite (dw, pw) ->
      Printf.sprintf "(ListStorageType.Composite (%u, %u))" dw pw


