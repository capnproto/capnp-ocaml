
module Bitfield = struct
  let tag_shift      = 62
  let tag_mask       = Uint64.shift_left (Uint64.of_int 0x3) tag_shift
  let tag_val_list   = Uint64.shift_left Uint64.one          tag_shift
  let tag_val_struct = Uint64.shift_left Uint64.zero         tag_shift
  let tag_val_far    = Uint64.shift_left (Uint64.of_int 0x2) tag_shift
end

type t = 
  | Null
  | List   of ListPointer.t
  | Struct of StructPointer.t
  | Far    of FarPointer.t


