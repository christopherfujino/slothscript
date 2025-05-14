open OUnit2

let parser_tests =
  "parser" >::: [ ("todo" >:: fun _ -> assert_equal true true) ]

let () = run_test_tt_main parser_tests
