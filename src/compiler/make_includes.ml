#!/usr/bin/env ocaml

(* Generate ocaml code containing the content of a couple of files, formatted
   as a list of lines.  This is used to perform an ocaml source inclusion,
   providing functor-like capability without the performance hit. *)

#use "topfind"
#require "core_kernel"

open Core_kernel


let make_inclusion oc variable_name filename =
  Out_channel.output_string oc ("let " ^ variable_name ^ " = [\n");
  In_channel.with_file filename ~f:(fun ic ->
    In_channel.iter_lines ic ~f:(fun line ->
      if String.trim line = "INCLUDE \"commonInc.ml\"" then
        In_channel.with_file "../runtime/commonInc.ml" ~f:(fun ic ->
          In_channel.iter_lines ic ~f:(fun line ->
            Out_channel.output_string oc "  \"  ";
            Out_channel.output_string oc (String.escaped line);
            Out_channel.output_string oc "\";\n"))
      else begin
        Out_channel.output_string oc "  \"";
        Out_channel.output_string oc (String.escaped line);
        Out_channel.output_string oc "\";\n"
      end));
  Out_channel.output_string oc "]\n\n"


let () =
  Out_channel.with_file "includes.ml" ~f:(fun oc ->
    make_inclusion oc "reader_api" "../runtime/readerInc.ml";
    make_inclusion oc "builder_api" "../runtime/builderInc.ml")

