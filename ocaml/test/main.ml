open OUnit2
open Sloth_script

type test_spec = { name : string; program : string; ast : string }

let test_specs =
  [
    { name = "num literal"; program = "11;"; ast = "(ExprStmt (Num 11))" };
    { name = "addition"; program = "1 + 1;"; ast = "(ExprStmt (Add (Num 1) (Num 1)))" };
    { name = "assignment"; program = "let x = 1 + 1;"; ast = "(LetStmt \"x\" (Add (Num 1) (Num 1)))"}
  ]

let parser_tests =
  let make_test name program expectation =
    name >:: fun _ ->
    let stmt = Compiler.parse_line program in
    assert_equal ~printer:(fun s -> s) expectation (Ast.stmt_to_str stmt)
  in
  "parser"
  >::: List.map
         (fun spec -> make_test spec.name spec.program spec.ast)
         test_specs

let () = run_test_tt_main parser_tests
