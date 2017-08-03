[@@@ocaml.warning "-3"]

module CamlBytes = Bytes
open Core.Std
module IO = Capnp.IO
module Codecs = Capnp.Codecs

let message_of_builder = Capnp.BytesMessage.StructStorage.message_of_builder

(* This is a wrapper for writing to a file descriptor and simultaneously
   counting the number of bytes written. *)
module CountingOutputStream = struct
  type t = {
    fd : Unix.File_descr.t;
    mutable throughput : int;
  }

  let write chan ~buf ~pos ~len =
    let bytes_written = Unix.single_write ~restart:true ~buf ~pos ~len chan.fd in
    let () = chan.throughput <- chan.throughput + bytes_written in
    bytes_written

  let wrap_write_context ~compression stream =
    IO.WriteContext.create ~write ~compression stream
end


module type BENCHMARK_SIG = sig
  val sync_client : input_fd:Unix.File_descr.t -> output_fd:Unix.File_descr.t ->
    compression:Codecs.compression_t -> iters:int -> int

  val async_client : input_fd:Unix.File_descr.t -> output_fd:Unix.File_descr.t ->
    compression:Codecs.compression_t -> iters:int -> int

  val server : input_fd:Unix.File_descr.t -> output_fd:Unix.File_descr.t ->
    compression:Codecs.compression_t -> iters:int -> int

  val pass_by_object : iters:int -> int

  val pass_by_bytes : compression:Codecs.compression_t -> iters:int -> int
end

module Benchmark
    (TestCase : TestCaseSig.TEST_CASE)
    (RequestReader : TestCaseSig.READER with type struct_t = TestCase.request_t)
    (ResponseReader : TestCaseSig.READER with type struct_t = TestCase.response_t)
    : BENCHMARK_SIG
= struct

  (* [sync_client] issues a randomized request and waits for the response,
     looping up to the specified number of iterations. *)
  let sync_client
      ~(input_fd : Unix.File_descr.t)
      ~(output_fd : Unix.File_descr.t)
      ~(compression : Codecs.compression_t)
      ~(iters : int)
    : int =
    let in_context = IO.create_read_context_for_fd ~compression input_fd in
    let out_stream = {
      CountingOutputStream.fd = output_fd;
      CountingOutputStream.throughput = 0;
    } in
    let out_context = CountingOutputStream.wrap_write_context ~compression out_stream in
    for _i = 0 to iters - 1 do
      let (request, expectation) = TestCase.setup_request () in
      let req_message = message_of_builder request in
      IO.WriteContext.write_message out_context req_message;
      match IO.ReadContext.read_message in_context with
      | Some resp_message ->
          let response = ResponseReader.of_message resp_message in
          if not (TestCase.check_response response expectation) then
            failwith "incorrect response."
          else
            ()
      | None ->
          failwith "EOF before message was received."
    done;
    out_stream.CountingOutputStream.throughput


  (* [async_client] issues randomized requests in a pipelined manner, matching
     up the corresponding responses asynchronously.  Unlike the capnproto C++
     benchmark, this runs in a single thread and uses a [select] loop to
     determine appropriate times to write and read. *)
  let async_client
      ~(input_fd : Unix.File_descr.t)
      ~(output_fd : Unix.File_descr.t)
      ~(compression : Codecs.compression_t)
      ~(iters : int)
    : int =
    let () = Unix.set_nonblock output_fd in
    let in_context = IO.create_read_context_for_fd ~compression input_fd in
    let out_stream = {
      CountingOutputStream.fd = output_fd;
      CountingOutputStream.throughput = 0;
    } in
    let out_context = CountingOutputStream.wrap_write_context ~compression out_stream in

    let expectations = Queue.create () in

    let num_sent = ref 0 in
    let final_send_complete = ref false in
    while !num_sent < iters || (not (Queue.is_empty expectations)) do
      let write_watch_fds = if !final_send_complete then [] else [output_fd] in
      let ready = Unix.select ~restart:true
          ~read:[input_fd] ~write:write_watch_fds ~except:[input_fd] ~timeout:`Never ()
      in

      if not (List.is_empty ready.Unix.Select_fds.read) then begin
        let (_ : int) = IO.ReadContext.read in_context in
        let rec loop () =
          match IO.ReadContext.dequeue_message in_context with
          | Some resp_message ->
              let response = ResponseReader.of_message resp_message in
              let expect = Queue.dequeue_exn expectations in
              if not (TestCase.check_response response expect) then
                failwith "incorrect response."
              else
                loop ()
          | None ->
              ()
        in
        loop ()
      end;

      if not (List.is_empty ready.Unix.Select_fds.write) then begin
        begin try
          while IO.WriteContext.write out_context > 0 do () done
        with
        | Unix.Unix_error (Unix.EAGAIN, _, _)
        | Unix.Unix_error (Unix.EWOULDBLOCK, _, _) ->
          ()
        end;
        let bytes_remaining = IO.WriteContext.bytes_remaining out_context in
        if !num_sent = iters then
          if bytes_remaining = 0 then
            final_send_complete := true
          else
            ()
        (* A large queue isn't actually helpful here, it just increases
           GC pressure. *)
        else if Queue.length expectations < 4 then begin
          let (request, expect) = TestCase.setup_request () in
          let req_message = message_of_builder request in
          IO.WriteContext.enqueue_message out_context req_message;
          Queue.enqueue expectations expect;
          num_sent := !num_sent + 1
        end
      end
    done;
    out_stream.CountingOutputStream.throughput


  (* [server] receives incoming requests one at a time, immediately writing
     a response for each request in turn. *)
  let server
      ~(input_fd : Unix.File_descr.t)
      ~(output_fd : Unix.File_descr.t)
      ~(compression : Codecs.compression_t)
      ~(iters : int)
    : int =
    let in_context = IO.create_read_context_for_fd ~compression input_fd in
    let out_stream = {
      CountingOutputStream.fd = output_fd;
      CountingOutputStream.throughput = 0;
    } in
    let out_context = CountingOutputStream.wrap_write_context ~compression out_stream in
    for _i = 0 to iters - 1 do
      match IO.ReadContext.read_message in_context with
      | Some req_message ->
          let request = RequestReader.of_message req_message in
          let response = TestCase.handle_request request in
          let resp_message = message_of_builder response in
          IO.WriteContext.write_message out_context resp_message
      | None ->
          failwith "EOF before all messages were read."
    done;
    out_stream.CountingOutputStream.throughput


  (* [pass_by_object] constructs a randomized request and generates a response for
     the request, looping up to the specified number of iterations.  Everything
     happens synchronously in one process, and no serialization takes place. *)
  let pass_by_object ~(iters : int) : int =
    let object_size_counter = ref 0 in
    for _i = 0 to iters - 1 do
      let (req_builder, expectation) = TestCase.setup_request () in
      let resp_builder = TestCase.handle_request
          (Capnp.BytesMessage.StructStorage.reader_of_builder req_builder)
      in
      if not (TestCase.check_response (Capnp.BytesMessage.StructStorage.reader_of_builder resp_builder)
            expectation) then
        failwith "incorrect response."
      else
        ();
      object_size_counter := !object_size_counter +
        (Capnp.BytesMessage.Message.total_size
           (message_of_builder req_builder)) +
        (Capnp.BytesMessage.Message.total_size
           (message_of_builder resp_builder))
    done;
    !object_size_counter


(* [pass_by_bytes] constructs a randomized request and generates a response for
   the request, looping up to the specified number of iterations.  Everything
   happens synchronously in one process.  The request and response are converted
   from objects to strings and back, in both directions. *)
  let pass_by_bytes ~(compression : Codecs.compression_t) ~(iters : int) =
    let throughput = ref 0 in
    for _i = 0 to iters - 1 do
      let (req_builder, expectation) = TestCase.setup_request () in
      let flattened_request =
        let req_message = message_of_builder req_builder in
        Codecs.serialize ~compression req_message
      in
      throughput := !throughput + (String.length flattened_request);

      let req_stream = Codecs.FramedStream.of_string ~compression
          flattened_request
      in
      let flattened_response =
        match Codecs.FramedStream.get_next_frame req_stream with
        | Result.Ok req_message ->
            let resp_builder = TestCase.handle_request
                (RequestReader.of_message req_message)
            in
            let resp_message = message_of_builder resp_builder in
            Codecs.serialize ~compression resp_message
        | Result.Error _ ->
            failwith "failed to decode complete request."
      in
      throughput := !throughput + (String.length flattened_response);

      let resp_stream = Codecs.FramedStream.of_string ~compression
          flattened_response
      in
      match Codecs.FramedStream.get_next_frame resp_stream with
      | Result.Ok resp_message ->
          if not (TestCase.check_response (ResponseReader.of_message resp_message)
                expectation) then
            failwith "incorrect response."
          else
            ()
      | Result.Error _ ->
          failwith "failed to decode complete response."

    done;
    !throughput

end


(* [pass_by_pipe] forks off a child (client) process connected to the current
   (server) process by a pipe.  The [client_func] and [server_func] are then
   used to carry out a benchmark method using the provided pipe transport. *)
let pass_by_pipe client_func server_func : int =
  let (client_to_server_read, client_to_server_write) = Unix.pipe () in
  let (server_to_client_read, server_to_client_write) = Unix.pipe () in
  match Unix.fork () with
  | `In_the_child ->
      (* client *)
      Unix.close client_to_server_read;
      Unix.close server_to_client_write;

      let throughput = client_func ~input_fd:server_to_client_read
          ~output_fd:client_to_server_write
      in
      let tp64 = Int64.of_int throughput in
      let buf = CamlBytes.create 8 in
      let () = EndianBytes.LittleEndian.set_int64 buf 0 tp64 in
      let bytes_written = Unix.write client_to_server_write
          ~buf:(CamlBytes.unsafe_to_string buf)
      in
      assert (bytes_written = 8);
      exit 0
  | `In_the_parent child_pid ->
      (* server *)
      Unix.close client_to_server_write;
      Unix.close server_to_client_read;

      let throughput = server_func ~input_fd:client_to_server_read
          ~output_fd:server_to_client_write
      in

      let tp64_buf = String.create 8 in
      let bytes_read = Unix.read client_to_server_read ~buf:tp64_buf in
      assert (bytes_read = 8);
      let tp64 = EndianString.LittleEndian.get_int64 tp64_buf 0 in
      let throughput = throughput + (Int64.to_int_exn tp64) in
      Unix.close client_to_server_read;
      Unix.close server_to_client_write;

      let () = Unix.waitpid_exn child_pid in
      throughput


