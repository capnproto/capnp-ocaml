#use "topfind"
#require "topkg-jbuilder"

open Topkg

let () =
  Topkg_jbuilder.describe ()
    ~readmes:[Pkg.std_file "README.adoc"]
    ~change_logs:[Pkg.std_file "CHANGELOG.adoc"]
    ~licenses:[]
