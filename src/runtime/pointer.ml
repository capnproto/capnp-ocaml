(******************************************************************************
 * capnp-ocaml
 *
 * Copyright (c) 2013-2014, Paul Pelzl
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimer.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 ******************************************************************************)


module Bitfield = struct
  let tag_mask       = 0x3
  let tag_val_list   = 0x1
  let tag_val_struct = 0x0
  let tag_val_far    = 0x2
  let tag_val_other  = tag_mask
end

type t =
  | Null
  | List   of ListPointer.t
  | Struct of StructPointer.t
  | Far    of FarPointer.t
  | Other  of OtherPointer.t

let decode (pointer64 : int64) : t =
  if Util.is_int64_zero pointer64 then
    Null
  else
    let pointer_int = Int64.to_int pointer64 in
    let tag = pointer_int land Bitfield.tag_mask in
    (* OCaml won't match an int against let-bound variables,
       only against constants. *)
    match tag with
    | 0x0 ->  (* Bitfield.tag_val_struct *)
        Struct (StructPointer.decode pointer64)
    | 0x1 ->  (* Bitfield.tag_val_list *)
        List (ListPointer.decode pointer64)
    | 0x2 ->  (* Bitfield.tag_val_far *)
        Far (FarPointer.decode pointer64)
    | 0x3 ->  (* Bitfield.tag_val_other *)
        Other (OtherPointer.decode pointer64)
    | _ ->
        assert false
