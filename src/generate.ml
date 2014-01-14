
open Core.Std

module PS = GenCommon.PS
module R  = Runtime


let sig_s_header =
  "module type S = sig\n" ^
  "  type message_t\n\n" ^
  "  module AnyPointer : sig\n" ^
  "    type t\n" ^
  "  end\n\n"

let sig_s_footer =
  "end\n\n"

let functor_sig =
  "module Make (MessageWrapper : Message.S) :\n" ^
  "  (S with type message_t = Message.ro MessageWrapper.Message.t)\n\n"


let mod_header =
  "module Make (MessageWrapper : Message.S) = struct\n" ^
  "  let invalid_msg = Message.invalid_msg\n\n" ^
  "  module Reader_ = MessageReader.Make(MessageWrapper)\n" ^
  "  open Reader_\n\n" ^
  "  type message_t = ro Reader_.Message.t\n\n" ^
  "  module AnyPointer = struct\n" ^
  "    type t = ro Slice.t option\n" ^
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


let compile (request : PS.CodeGeneratorRequest.t) (dest_dir : string) : unit =
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
    let requested_filename = RequestedFile.filename_get requested_file in
    let sig_s =
      sig_s_header ^
      (GenSignatures.generate_node ~suppress_module_wrapper:true nodes_table []
        requested_file_node requested_filename) ^
      sig_s_footer
    in
    let sig_file_content = sig_s ^ functor_sig in
    let mod_file_content =
      sig_s ^
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


