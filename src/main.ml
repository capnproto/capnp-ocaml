
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
      let () = Printf.printf "consumed %d/%d bytes\n" bytes_consumed (String.length bytes) in
      let open M in
      let message = Message.readonly (Message.of_storage segments) in
      let request = PS.CodeGeneratorRequest.of_message message in
      let files = PS.CodeGeneratorRequest.requestedFiles_get request in
      let file_count = PS.List.length files in
      let () = Printf.printf "requested file count: %u\n" file_count in
      let first_file = PS.List.get files 0 in
      let first_filename = PS.CodeGeneratorRequest.RequestedFile.filename_get first_file in
      let () = Printf.printf "filename: \"%s\"\n" first_filename in
      ExitCode.success
  | Result.Error StrStorage.FramingError.Incomplete ->
      let () = Printf.printf "incomplete message\n" in
      ExitCode.general_error
  | Result.Error StrStorage.FramingError.Unsupported ->
      let () = Printf.printf "unsupported message\n" in
      ExitCode.general_error


let () = Pervasives.exit (main ())

