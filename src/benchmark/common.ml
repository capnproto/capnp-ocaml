
open Core.Std
module IO = Capnp.IO
module Codecs = Capnp.Codecs


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


module SyncClient
    (TestCase : TestCaseSig.TEST_CASE)
    (RequestBuilder : TestCaseSig.BUILDER with type t = TestCase.request_builder_t)
    (ResponseReader : TestCaseSig.READER with type t = TestCase.response_reader_t)
= struct
  let f
    ~(input_fd : Unix.File_descr.t)
    ~(output_fd : Unix.File_descr.t)
    ~(compression : Capnp.Codecs.compression_t)
    ~(iters : int)
  : int =
  let in_context = IO.create_read_context_for_fd ~compression input_fd in
  let out_stream = {
    CountingOutputStream.fd = output_fd;
    CountingOutputStream.throughput = 0;
  } in
  let out_context = CountingOutputStream.wrap_write_context ~compression out_stream in
  for i = 0 to iters - 1 do
    let (request, expectation) = TestCase.setup_request () in
    let req_message = RequestBuilder.to_message request in
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

end


module Server
    (TestCase : TestCaseSig.TEST_CASE)
    (RequestReader : TestCaseSig.READER with type t = TestCase.request_reader_t)
    (ResponseBuilder : TestCaseSig.BUILDER with type t = TestCase.response_builder_t)
= struct
  let f
    ~(input_fd : Unix.File_descr.t)
    ~(output_fd : Unix.File_descr.t)
    ~(compression : Capnp.Codecs.compression_t)
    ~(iters : int)
  : int =
  let in_context = IO.create_read_context_for_fd ~compression input_fd in
  let out_stream = {
    CountingOutputStream.fd = output_fd;
    CountingOutputStream.throughput = 0;
  } in
  let out_context = CountingOutputStream.wrap_write_context ~compression out_stream in
  for i = 0 to iters - 1 do
    match IO.ReadContext.read_message in_context with
    | Some req_message ->
        let request = RequestReader.of_message req_message in
        let response = TestCase.handle_request request in
        let resp_message = ResponseBuilder.to_message response in
        IO.WriteContext.write_message out_context resp_message
    | None ->
        failwith "EOF before all messages were read."
  done;
  out_stream.CountingOutputStream.throughput

end


