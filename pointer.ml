
module Bitfield = struct
  let tag_shift      = 62
  let tag_mask       = Int64.shift_left (Int64.of_int 0x3) tag_shift
  let tag_val_list   = Int64.shift_left Int64.one          tag_shift
  let tag_val_struct = Int64.shift_left Int64.zero         tag_shift
  let tag_val_far    = Int64.shift_left (Int64.of_int 0x2) tag_shift
end

type t = 
  | Null
  | List   of ListPointer.t
  | Struct of StructPointer.t
  | Far    of FarPointer.t


