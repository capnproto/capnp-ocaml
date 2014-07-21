
let a = 1664525
let c = 1013904223
let state = ref c

let next () =
  state := ((a * !state) + c) land 0xffffffff;
   !state

let int range =
  (next ()) mod range

let float_of_uint32_max = float_of_int 0xffffffff

let double range =
  (float_of_int (next ())) *. range /. float_of_uint32_max


