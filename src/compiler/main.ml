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


open Core_kernel.Std
open Capnp

module M  = BytesMessage
module PS = GenCommon.PS
module IO = Capnp_unix.IO

module ExitCode = struct
  let success       = 0
  let general_error = 1
  let syntax_error  = 2
end

let main () : int =
  let () = In_channel.set_binary_mode In_channel.stdin true in
  try
    begin match IO.read_single_message_from_channel
                  ~compression:`None In_channel.stdin with
    | Some message ->
        let message = M.Message.readonly message in
        let request = PS.CodeGeneratorRequest.of_message message in
        begin try
          let () = Generate.compile request in
          ExitCode.success
        with (Failure _) as e ->
          let bs = Exn.backtrace () in
          let es = Exn.to_string e in
          let () = prerr_endline es in
          let () = prerr_endline bs in
          ExitCode.general_error
        end
    | None ->
        let () = Printf.printf
            "Could not decode a complete CodeGeneratorRequest message.\n"
        in
        ExitCode.general_error
    end
  with IO.Unsupported_message_frame ->
      let () = Printf.printf
          "CodeGeneratorRequest message header describes unsupported message size.\n"
      in
      ExitCode.general_error


let () = Pervasives.exit (main ())

