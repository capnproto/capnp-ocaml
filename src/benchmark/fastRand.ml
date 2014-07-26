
let a = 1664525
let c = 1013904223
let state = ref c

let x = ref 0x1d2acd47
let y = ref 0x58ca3e14
let z = ref 0xf563f232
let w = ref 0x0bc76199

let next () =
  let tmp = !x lxor (!x lsl 11) in
  x := !y;
  y := !z;
  z := !w;
  w := !w lxor (!x lsr 19) lxor tmp lxor (tmp lsr 8);
  w := !w land 0xffffffff;
  !w

let int range =
  (next ()) mod range

let float_of_uint32_max = float_of_int 0xffffffff

let double range =
  (float_of_int (next ())) *. range /. float_of_uint32_max


