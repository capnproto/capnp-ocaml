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

module PS   = GenCommon.PS
module C    = Capnp
module Mode = GenCommon.Mode


let sig_s_header = [
  "type ro = Capnp.Message.ro";
  "type rw = Capnp.Message.rw";
  "";
  "module type S = sig";
  "  type 'cap message_t";
  "";
]

let sig_s_reader_header = [
  "";
  "  module Reader : sig";
  "    type array_t";
  "    type builder_array_t";
  "    type pointer_t";
]

let sig_s_divide_reader_builder = [
  "  end";
  "";
  "  module Builder : sig";
  "    type array_t = Reader.builder_array_t";
  "    type reader_array_t = Reader.array_t";
  "    type pointer_t";
]

let sig_s_footer = [
  "  end";
  "end";
  "";
]


let functor_sig = [
  "module Make (MessageWrapper : Capnp.Message.S) :";
  "  (S with type 'cap message_t = 'cap MessageWrapper.Message.t";
  "    and type Reader.pointer_t = ro MessageWrapper.Slice.t option";
  "    and type Builder.pointer_t = rw MessageWrapper.Slice.t)";
  "";
]

let mod_header = [
  "module Make (MessageWrapper : Capnp.Message.S) = struct";
  "  open Capnp";
  "";
  "  let invalid_msg = Message.invalid_msg";
  "";
  "  module RA_ = Runtime.Reader.Make(MessageWrapper)";
  "  module BA_ = Runtime.Builder.Make(MessageWrapper)";
  "";
  "  type 'cap message_t = 'cap MessageWrapper.Message.t";
  "";
]

let mod_reader_header = [
  "";
  "  module Reader = struct";
  "    type array_t = ro RA_.ListStorage.t";
  "    type builder_array_t = rw RA_.ListStorage.t";
  "    type pointer_t = ro MessageWrapper.Slice.t option";
  "";
]

let mod_divide_reader_builder = [
  "  end";
  "";
  "  module Builder = struct";
  "    type array_t = Reader.builder_array_t";
  "    type reader_array_t = Reader.array_t";
  "    type pointer_t = rw MessageWrapper.Slice.t";
  "";
]

let mod_footer = [
  "  end";
  "end";
]


let module_filename filename =
  filename |>
  Filename.basename |>
  Filename.chop_extension |>
  String.tr ~target:'-' ~replacement:'_' |>
  String.uncapitalize


let ml_filename filename  = (module_filename filename) ^ ".ml"
let mli_filename filename = (module_filename filename) ^ ".mli"


let string_of_lines lines =
  (String.concat ~sep:"\n" lines) ^ "\n"


let compile
    (request : PS.CodeGeneratorRequest.t)
  : unit =
  let nodes_table = Hashtbl.Poly.create () in
  let nodes = PS.CodeGeneratorRequest.nodes_get request in
  let () = C.Array.iter nodes ~f:(fun node ->
      Hashtbl.replace nodes_table ~key:(PS.Node.id_get node) ~data:node)
  in
  let requested_files = PS.CodeGeneratorRequest.requested_files_get request in
  C.Array.iter requested_files ~f:(fun requested_file ->
    let open PS.CodeGeneratorRequest in
    let requested_file_id = RequestedFile.id_get requested_file in
    let requested_file_node = Hashtbl.find_exn nodes_table requested_file_id in
    let requested_filename = RequestedFile.filename_get requested_file in
    let sig_unique_types = List.rev_map
        (GenCommon.collect_unique_types ~nodes_table requested_file_node)
        ~f:(fun (name, tp) -> "  type " ^ name)
    in
    let sig_unique_enums =
      GenCommon.apply_indent ~indent:"  "
        (GenCommon.collect_unique_enums ~is_sig:true ~nodes_table
           ~node_name:requested_filename requested_file_node)
    in
    let mod_unique_types = (List.rev_map
        (GenCommon.collect_unique_types ~nodes_table requested_file_node)
        ~f:(fun (name, tp) -> "  type " ^ name ^ " = " ^ tp)) @ [""]
    in
    let mod_unique_enums =
      GenCommon.apply_indent ~indent:"  "
        (GenCommon.collect_unique_enums ~is_sig:false ~nodes_table
           ~node_name:requested_filename requested_file_node)
    in
    let sig_s =
      sig_s_header @
      sig_unique_types @
      sig_unique_enums @
      sig_s_reader_header @
      (GenCommon.apply_indent ~indent:"    "
        (GenSignatures.generate_node ~suppress_module_wrapper:true ~nodes_table
           ~scope:[] ~mode:Mode.Reader ~node_name:requested_filename
           requested_file_node)) @
      sig_s_divide_reader_builder @
      (GenCommon.apply_indent ~indent:"    "
        (GenSignatures.generate_node ~suppress_module_wrapper:true ~nodes_table
           ~scope:[] ~mode:Mode.Builder ~node_name:requested_filename
           requested_file_node)) @
      sig_s_footer
    in
    let sig_file_content =
      string_of_lines (sig_s @ functor_sig)
    in
    let mod_file_content =
      let defaults_context =
        GenModules.build_defaults_context ~nodes_table ~node_name:requested_filename
        requested_file_node
      in
      let reader_body =
        GenCommon.apply_indent ~indent:"    "
          (GenModules.generate_node ~suppress_module_wrapper:true ~nodes_table
             ~scope:[] ~mode:Mode.Reader ~node_name:requested_filename
             requested_file_node)
      in
      let builder_body =
        GenCommon.apply_indent ~indent:"    "
          (GenModules.generate_node ~suppress_module_wrapper:true ~nodes_table
             ~scope:[] ~mode:Mode.Builder ~node_name:requested_filename
             requested_file_node)
      in
      let builder_defaults = Defaults.gen_builder_defaults defaults_context in
      let reader_defaults = GenCommon.apply_indent ~indent:"  "
          (Defaults.gen_reader_defaults defaults_context)
      in
      string_of_lines (
        sig_s @
        builder_defaults @
        mod_header @
        mod_unique_types @
        mod_unique_enums @
        reader_defaults @
        mod_reader_header @
        reader_body @
        mod_divide_reader_builder @
        builder_body @
        mod_footer)
    in
    let () = Out_channel.with_file (mli_filename requested_filename)
        ~f:(fun chan -> Out_channel.output_string chan sig_file_content)
    in
    let () = Out_channel.with_file (ml_filename requested_filename)
        ~f:(fun chan -> Out_channel.output_string chan mod_file_content)
    in
    ())


