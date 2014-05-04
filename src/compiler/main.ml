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


open Core.Std
open Capnp

module M  = GenCommon.M
module PS = GenCommon.PS

module ExitCode = struct
  let success       = 0
  let general_error = 1
  let syntax_error  = 2
end

let main () : int =
  let () = In_channel.set_binary_mode In_channel.stdin true in
  let bytes = In_channel.input_all In_channel.stdin in
  match StringStorage.unpack_single_frame bytes with
  | Result.Ok (segments, bytes_consumed) ->
      let open M in
      let message = Message.readonly (Message.of_storage segments) in
      let request = PS.CodeGeneratorRequest.of_message message in
      begin try
        let () = Generate.compile request "dir_name" in
        ExitCode.success
      with (Failure msg) as e ->
        let bs = Exn.backtrace () in
        let es = Exn.to_string e in
        let () = prerr_endline es in
        let () = prerr_endline bs in
        ExitCode.general_error
      end
  | Result.Error StringStorage.FramingError.Incomplete ->
      let () = Printf.printf "incomplete message\n" in
      ExitCode.general_error
  | Result.Error StringStorage.FramingError.Unsupported ->
      let () = Printf.printf "unsupported message\n" in
      ExitCode.general_error


let () = Pervasives.exit (main ())

