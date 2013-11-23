
open Core.Std

module M  = Message.Make(StrStorage)
module PS = PluginSchema.Make(StrStorage)

module ExitCode = struct
  let success       = 0
  let general_error = 1
  let syntax_error  = 2
end

let main () : int =
  let () = In_channel.set_binary_mode In_channel.stdin true in
  let bytes = In_channel.input_all In_channel.stdin in
  match StrStorage.unpack_single_frame bytes with
  | Result.Ok (segments, bytes_consumed) ->
      let open M in
      let message = Message.readonly (Message.of_storage segments) in
      let request = PS.CodeGeneratorRequest.of_message message in
      let () = Generate.compile request "dir_name" in
      ExitCode.success
  | Result.Error StrStorage.FramingError.Incomplete ->
      let () = Printf.printf "incomplete message\n" in
      ExitCode.general_error
  | Result.Error StrStorage.FramingError.Unsupported ->
      let () = Printf.printf "unsupported message\n" in
      ExitCode.general_error


let () = Pervasives.exit (main ())

