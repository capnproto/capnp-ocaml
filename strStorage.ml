open Core.Std
open EndianString

type t = string

let create = String.create
let length = String.length

let get_uint8      = LittleEndian.get_uint8
let get_uint16     = LittleEndian.get_uint16
let get_uint32 s i = Uint32.of_int32 (LittleEndian.get_int32 s i)
let get_uint64 s i = Uint64.of_int64 (LittleEndian.get_int64 s i)

let get_int8       = LittleEndian.get_int8
let get_int16      = LittleEndian.get_int16
let get_int32      = LittleEndian.get_int32
let get_int64      = LittleEndian.get_int64

(* Bug in ocplib-endian: [set_int8] behaves like [set_uint8] *)
let set_uint8        = LittleEndian.set_int8

let set_uint32 s i v = LittleEndian.set_int32 s i (Uint32.to_int32 v)
let set_uint64 s i v = LittleEndian.set_int64 s i (Uint64.to_int64 v)

let set_int16        = LittleEndian.set_int16
let set_int32        = LittleEndian.set_int32
let set_int64        = LittleEndian.set_int64


let set_int8 s i v =
  if v < -128 || v > 127 then
    invalid_arg "StrStorage.set_int8"
  else
    let u8 =
      if v < 0 then
        (1 lsl 8) - (-v)
      else
        v
    in
    (* Bug in ocplib-endian: [set_int8] behaves like [set_uint8] *)
    LittleEndian.set_int8 s i u8


let set_uint16 s i v =
  if v < 0 || v > 0xffff then
    invalid_arg "StrStorage.set_uint16"
  else
    let i16 =
      if v > 32767 then
        let comp = (1 lsl 8) - v in
        -comp
      else
        v
    in
    LittleEndian.set_int16 s i i16


module FramingError = struct
  type t =
    | Incomplete    (* less than a full frame is available *)
    | Unsupported   (* frame describes a segment count or segment size that is too large *)
end

(** Given a string containing a framed message, construct a sequence of
 *  string message segments.
 *
 *  Returns: Ok (segments, bytes_consumed) if successful,
 *           otherwise an error code
 *)
let unpack_single_frame (s : string) : ((string list * int), FramingError.t) Result.t =
  let word_size = 8 in
  if String.length s < 4 then
    Result.Error FramingError.Incomplete
  else
    let segment_count_u32 = get_uint32 s 0 in
    begin try
      let segment_count     = 1 + (Uint32.to_int segment_count_u32) in
      let frame_header_size = (Util.ceil_int (4 * (segment_count + 1)) word_size) * word_size in
      if String.length s < frame_header_size then
        Result.Error FramingError.Incomplete
      else
        let arr = Res.Array.empty () in
        let ofs = ref frame_header_size in
        let () =
          for i = 0 to segment_count - 1 do
            let segment_size_words_u32 = get_uint32 s (4 + (4 * i)) in
            let segment_size = word_size * (Uint32.to_int segment_size_words_u32) in
            if !ofs + segment_size > String.length s then
              invalid_arg "incomplete segment"
            else
              let () = Res.Array.add_one arr (String.slice s !ofs (!ofs + segment_size)) in
              ofs := !ofs + segment_size
          done
        in
        Result.Ok (Res.Array.to_list arr, !ofs)
    with Invalid_argument _ ->
      Result.Error FramingError.Unsupported
    end


