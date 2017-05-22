open OUnit2

let () = run_test_tt_main TestEncoding.encoding_suite

let () = run_test_tt_main TestBytesStorage.uint8_suite
let () = run_test_tt_main TestBytesStorage.uint16_suite
let () = run_test_tt_main TestBytesStorage.uint32_suite
let () = run_test_tt_main TestBytesStorage.uint64_suite
let () = run_test_tt_main TestBytesStorage.int8_suite
let () = run_test_tt_main TestBytesStorage.int16_suite
let () = run_test_tt_main TestBytesStorage.int32_suite
let () = run_test_tt_main TestBytesStorage.int64_suite

let () = run_test_tt_main TestCodecs.packing_suite
let () = run_test_tt_main TestCodecs.random_packing_suite
let () = run_test_tt_main TestCodecs.random_serialize_suite

let () = run_test_tt_main TestMisc.encode_signed_suite
let () = run_test_tt_main TestMisc.decode_signed_suite
