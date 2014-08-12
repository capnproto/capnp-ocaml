
module CR :
  Catrank.S with type 'cap message_t = 'cap Capnp.BytesMessage.Message.t
= struct
  module MessageWrapper = Capnp.BytesMessage
  INCLUDE "catrank_defun.ml"
end

open Core.Std

module TestCase = struct
  type request_reader_t   = CR.Reader.SearchResultList.t
  type request_builder_t  = CR.Builder.SearchResultList.t
  type response_reader_t  = CR.Reader.SearchResultList.t
  type response_builder_t = CR.Builder.SearchResultList.t
  type expectation_t      = int

  let url_prefix = "http://example.com/"
  let url_prefix_len = String.length url_prefix

  let words = [|
    "foo "; "bar "; "baz "; "qux "; "quux "; "corge ";
    "grault "; "garply "; "waldo "; "fred "; "plugh ";
    "xyzzy "; "thud "; |]


  (* For this workload, calling into glibc 'strstr' significantly outperforms
     the KMP algorithm found in Core_string. *)
  external _string_contains : string -> string -> bool =
    "capnp_bench_string_contains" "noalloc"
  let string_contains ~haystack ~needle = _string_contains haystack needle


  let setup_request () =
    let builder = CR.Builder.SearchResultList.init_root () in
    let num_results = FastRand.int 1000 in
    let results = CR.Builder.SearchResultList.results_init builder num_results in
    let good_count = ref 0 in
    let open CR.Builder.SearchResult in
    for i = 0 to num_results - 1 do
      let result = Capnp.Array.get results i in
      score_set result (Float.of_int (1000 - i));
      let url_size = FastRand.int 100 in
      let url_total_size = url_size + url_prefix_len in
      let url = Bytes.create url_total_size in
      Bytes.blit
        (Bytes.unsafe_of_string url_prefix) 0
        url 0
        url_prefix_len;
      for j = 0 to url_size - 1 do
        let char_ofs = FastRand.int 26 in
        let byte = (Char.to_int 'a') + char_ofs in
        Bytes.unsafe_set url (url_prefix_len + j) (Char.unsafe_of_int byte)
      done;
      CR.Builder.SearchResult.url_set result url;

      let is_cat = FastRand.int 8 = 0 in
      let is_dog = FastRand.int 8 = 0 in
      good_count := !good_count +
          (Capnp.Runtime.Util.int_of_bool (is_cat && (not is_dog)));

      let snippet = Buffer.create (40 * 6) in
      Buffer.add_char snippet ' ';
      let prefix = FastRand.int 20 in
      for j = 0 to prefix - 1 do
        let word_index = FastRand.int (Array.length words) in
        Buffer.add_string snippet words.(word_index)
      done;

      if is_cat then
        Buffer.add_string snippet "cat ";
      if is_dog then
        Buffer.add_string snippet "dog ";

      let suffix = FastRand.int 20 in
      for j = 0 to suffix - 1 do
        let word_index = FastRand.int (Array.length words) in
        Buffer.add_string snippet words.(word_index)
      done;

      snippet_set result (Buffer.contents snippet)
    done;
    (builder, !good_count)

  module ScoredResult = struct
    type t = {
      score  : float;
      result : CR.Reader.SearchResult.t;
    }

    let compare a b = Float.compare a.score b.score
  end


  let handle_request result_list =
    let module R = CR.Reader.SearchResult in
    let module B = CR.Builder.SearchResult in

    let results = CR.Reader.SearchResultList.results_get result_list in
    let num_results = Capnp.Array.length results in

    if num_results = 0 then
      CR.Builder.SearchResultList.init_root ()
    else begin
      let result0 = Capnp.Array.get results 0 in
      let scored_results = Array.create ~len:num_results
          { ScoredResult.score = 0.0; ScoredResult.result = result0; }
      in
      for i = 0 to num_results - 1 do
        let result = Capnp.Array.get results i in
        let score = ref (R.score_get result) in
        let snippet = R.snippet_get result in

        if string_contains ~haystack:snippet ~needle:" cat " then
          score := !score *. 10000.0;
        if string_contains ~haystack:snippet ~needle:" dog " then
          score := !score /. 10000.0;

        scored_results.(i) <- {
          ScoredResult.score = !score;
          ScoredResult.result;
        }
      done;

      Array.sort scored_results ~cmp:ScoredResult.compare;

      let response = CR.Builder.SearchResultList.init_root () in
      let results = CR.Builder.SearchResultList.results_init response num_results in
      for i = 0 to num_results - 1 do
        let src = scored_results.(i) in
        let dest_result = Capnp.Array.get results i in
        B.score_set dest_result src.ScoredResult.score;
        B.url_set dest_result (R.url_get src.ScoredResult.result);
        B.snippet_set dest_result (R.snippet_get src.ScoredResult.result)
      done;
      response
    end

  let check_response result_list expected =
    let good_count = ref 0 in
    let results = CR.Reader.SearchResultList.results_get result_list in
    let num_results = Capnp.Array.length results in
    for i = 0 to num_results - 1 do
      let result = Capnp.Array.get results i in
      if CR.Reader.SearchResult.score_get result > 1001.0 then
        good_count := !good_count + 1
    done;
    !good_count = expected

end

