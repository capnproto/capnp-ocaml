
module Bitfield = struct
  let tag_mask       = Int64.of_int 0x3
  let tag_val_list   = Int64.one
  let tag_val_struct = Int64.zero
  let tag_val_far    = (Int64.of_int 0x2)
end

type t = 
  | Null
  | List   of ListPointer.t
  | Struct of StructPointer.t
  | Far    of FarPointer.t


