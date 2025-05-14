open OUnit2

let tests = [ ("todo" >:: fun _ -> assert_equal true true) ]
let () = run_test_tt_main ("suite" >::: tests)
