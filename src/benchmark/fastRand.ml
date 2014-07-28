
(* Double-secret undocumented API for direct calls to C functions which
   do not allocate from the OCaml heap *)
external next : unit -> int = "capnp_bench_nextFastRand" "noalloc"
external int  : int  -> int = "capnp_bench_fastRand"     "noalloc"

(* Double-secret undocumented API for direct calls to functions on doubles *)
external double : float -> float =
  "capnp_bench_fastRandDouble" "capnp_bench_unboxed_fastRandDouble" "float"

