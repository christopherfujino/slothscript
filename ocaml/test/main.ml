open OUnit2
open Sloth_script

let parser_tests =
  let make_test name program expectation =
    name >:: fun _ ->
    let stmt = Compiler.parse_line program in
    assert_equal ~printer:(fun s -> s) expectation (Ast.stmt_to_str stmt)
  in
  "parser"
  >::: [
         make_test "num literal" "11" "(ExprStmt 11)";
         make_test "addition" "1 + 1" "TODO";
       ]

let () = run_test_tt_main parser_tests
