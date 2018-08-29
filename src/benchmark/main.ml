
open Core_kernel

let printf = Printf.printf
let fprintf = Printf.fprintf


module BenchmarkRunner(BM : Methods.BENCHMARK_SIG) = struct

  let f mode compression iters =
    if mode = "client" then
      BM.sync_client ~input_fd:Unix.stdin ~output_fd:Unix.stdout
        ~compression ~iters
    else if mode = "server" then
      BM.server ~input_fd:Unix.stdin ~output_fd:Unix.stdout
        ~compression ~iters
    else if mode = "object" then
      BM.pass_by_object ~iters
    else if mode = "bytes" then
      BM.pass_by_bytes ~compression ~iters
    else if mode = "pipe" then
      Methods.pass_by_pipe
        (BM.sync_client ~compression ~iters)
        (BM.server ~compression ~iters)
    else if mode = "pipe-async" then
      Methods.pass_by_pipe
        (BM.async_client ~compression ~iters)
        (BM.server ~compression ~iters)
    else begin
      fprintf stderr "Unknown mode: \"%s\"\n" mode;
      exit 1
    end

end


let () =
  if Array.length Sys.argv <> 4 then begin
    fprintf stderr "USAGE:  %s MODE COMPRESSION ITERATION_COUNT\n"
      Sys.argv.(0);
    exit 1
  end;

  let name   = Filename.basename Sys.argv.(0) in
  let mode   = Sys.argv.(1) in
  let comp_s = Sys.argv.(2) in
  let iter_s = Sys.argv.(3) in

  let iters = Int.of_string iter_s in

  let compression =
    if comp_s = "none" then
      `None
    else if comp_s = "packed" then
      `Packing
    else begin
      fprintf stderr "Unknown compression mode \"%s\".\n" comp_s;
      exit 1
    end
  in

  let throughput =
    if name = "carsales" then
      (* Carsales benefits a small amount from tuning the GC to
         increase the space overhead. *)
      let () = Gc.tune ~space_overhead:1000 () in
      let module BM = Methods.Benchmark
          (CapnpCarsales.TestCase)
          (CapnpCarsales.CS.Reader.ParkingLot)
          (CapnpCarsales.CS.Reader.TotalValue)
      in
      let module BR = BenchmarkRunner(BM) in
      BR.f mode compression iters
    else if name = "catrank" then
      (* Catrank allocates relatively large messages.  It benefits from
         tuning the GC to increase the space overhead and thus reduce
         the major collection rate. *)
      let () = Gc.tune ~space_overhead:10000 () in
      let module BM = Methods.Benchmark
          (CapnpCatrank.TestCase)
          (CapnpCatrank.CR.Reader.SearchResultList)
          (CapnpCatrank.CR.Reader.SearchResultList)
      in
      let module BR = BenchmarkRunner(BM) in
      BR.f mode compression iters
    else if name = "eval" then
      let module BM = Methods.Benchmark
          (CapnpEval.TestCase)
          (CapnpEval.E.Reader.Expression)
          (CapnpEval.E.Reader.EvaluationResult)
      in
      let module BR = BenchmarkRunner(BM) in
      BR.f mode compression iters
    else begin
      fprintf stderr "Unknown benchmark name \"%s\".\n" name;
      exit 1
    end
  in

  printf "%d\n" throughput;
  exit 0


