
open Core.Std

module PS = PluginSchema.Make(StrStorage)
module R  = Runtime


let sig_header =
  "module type S = sig\n" ^
  "  module AnyPointer : sig\n" ^
  "    type t\n" ^
  "  end\n\n"

let sig_footer =
  "end\n\n" ^
  "module Make (Storage : MessageStorage.S) : S\n\n"


let mod_header =
  "module Make (Storage : MessageStorage.S) = struct\n" ^
  "  let invalid_msg = Message.invalid_msg\n\n" ^
  "  module Reader_ = MessageReader.Make(Storage)\n" ^
  "  open Reader_\n\n" ^
  "  module AnyPointer = struct\n" ^
  "    type t = Message.ro Slice.t option\n" ^
  "  end\n\n"

let mod_footer =
  "end\n\n"


let module_filename filename =
  filename |>
  Filename.basename |>
  Filename.chop_extension |>
  String.tr ~target:'-' ~replacement:'_' |>
  String.uncapitalize


let ml_filename filename  = (module_filename filename) ^ ".ml"
let mli_filename filename = (module_filename filename) ^ ".mli"


let compile (request : Message.ro PS.CodeGeneratorRequest.t) (dest_dir : string) : unit =
  let nodes_table = Hashtbl.Poly.create () in
  let nodes = PS.CodeGeneratorRequest.nodes_get request in
  for i = 0 to R.Array.length nodes - 1 do
    let node = R.Array.get nodes i in
    Hashtbl.replace nodes_table ~key:(PS.Node.id_get node) ~data:node
  done;
  let requested_files = PS.CodeGeneratorRequest.requestedFiles_get request in
  for i = 0 to R.Array.length requested_files - 1 do
    let requested_file = R.Array.get requested_files i in
    let open PS.CodeGeneratorRequest in
    let requested_file_id = RequestedFile.id_get requested_file in
    let requested_file_node = Hashtbl.find_exn nodes_table requested_file_id in
    let requested_filename = RequestedFile.filename_get requested_file_node in
    let sig_file_content =
      sig_header ^
      (GenSignatures.generate_node ~suppress_module_wrapper:true nodes_table []
        requested_file_node requested_filename) ^
      sig_footer
    in
    let mod_file_content =
      mod_header ^
      (GenModules.generate_node ~suppress_module_wrapper:true nodes_table []
        requested_file_node requested_filename) ^
      mod_footer
    in
    let () = Out_channel.with_file (mli_filename requested_filename) ~f:(fun chan ->
      Out_channel.output_string chan sig_file_content)
    in
    let () = Out_channel.with_file (ml_filename requested_filename) ~f:(fun chan ->
      Out_channel.output_string chan mod_file_content)
    in
    ()
  done


