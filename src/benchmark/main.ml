
open Core.Std

let printf = Printf.printf
let fprintf = Printf.fprintf


let pass_by_pipe client_func server_func iters =
  let (client_to_server_read, client_to_server_write) = Unix.pipe () in
  let (server_to_client_read, server_to_client_write) = Unix.pipe () in
  match Unix.fork () with
  | `In_the_child ->
      (* client *)
      Unix.close client_to_server_read;
      Unix.close server_to_client_write;

      let throughput = client_func ~input_fd:server_to_client_read
          ~output_fd:client_to_server_write ~iters
      in
      let tp64 = Int64.of_int throughput in
      let buf = Bytes.create 8 in
      let () = EndianBytes.LittleEndian.set_int64 buf 0 tp64 in
      let bytes_written = Unix.write client_to_server_write
          ~buf:(Bytes.unsafe_to_string buf)
      in
      assert (bytes_written = 8);
      exit 0
  | `In_the_parent child_pid ->
      (* server *)
      Unix.close client_to_server_write;
      Unix.close server_to_client_read;

      let throughput = server_func ~input_fd:client_to_server_read
          ~output_fd:server_to_client_write ~iters
      in

      let tp64_buf = Bytes.create 8 in
      let bytes_read = Unix.read client_to_server_read ~buf:tp64_buf in
      assert (bytes_read = 8);
      let tp64 = EndianBytes.LittleEndian.get_int64 tp64_buf 0 in
      let throughput = throughput + (Int64.to_int_exn tp64) in
      Unix.close client_to_server_read;
      Unix.close server_to_client_write;

      let () = Unix.waitpid_exn child_pid in
      throughput


let () =
  (*
  if Array.length Sys.argv <> 4 then begin
    fprintf stderr "USAGE:  %s MODE COMPRESSION ITERATION_COUNT\n" argv.(0);
    exit 1
  end else
    ();
  *)

  let module C = Common.AsyncClient
      (CapnpCarsales.TestCase)
      (CapnpCarsales.CS.Builder.ParkingLot)
      (CapnpCarsales.CS.Reader.TotalValue)
  in

  let module S = Common.Server
      (CapnpCarsales.TestCase)
      (CapnpCarsales.CS.Reader.ParkingLot)
      (CapnpCarsales.CS.Builder.TotalValue)
  in

  let throughput = pass_by_pipe
    (C.f ~compression:`None)
    (S.f ~compression:`None)
    1000
  in
  printf "throughput: %d\n" throughput;
  exit 0


