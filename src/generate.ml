
open Core.Std

module PS = PluginSchema.Make(StrStorage)
module R  = Runtime


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
    let sig_file_content = GenSignatures.generate_node nodes_table []
      requested_file_node requested_filename
    in
    let mod_file_content = GenModules.generate_node nodes_table []
      requested_file_node requested_filename
    in
    let () = print_endline sig_file_content in
    let () = print_endline mod_file_content in
    ()
  done


