open OUnit2

type test_spec = { name : string; program : string; ast : string }

let test_specs =
  [
    { name = "num literal"; program = "11;"; ast = "(ExprStmt (Num 11))" };
    {
      name = "addition";
      program = "1 + 1;";
      ast = "(ExprStmt (Add (Num 1) (Num 1)))";
    };
    {
      name = "assignment";
      program = "let x = 1 + 1;";
      ast = "(LetStmt \"x\" (Add (Num 1) (Num 1)))";
    };
  ]

let tests =
  let make_parser_test name program expectation =
    let open Compiler in
    name >:: fun _ ->
    let stmt = Main.parse_line program in
    assert_equal ~printer:(fun s -> s) expectation (Ast.stmt_to_str stmt)
  in
  let make_interpreter_test name program =
    let open Compiler in
    name >:: fun _ ->
    let stmt = Main.parse_line program in
    let ctx =
      Interpreter.Context.make_ctx
        (module Interpreter.Sloth_stdlib.Make_test ())
    in
    Interpreter.Interpret.interpret_stmt ctx stmt;
    ()
  in
  "slothscript"
  >::: [
         "parser"
         >::: List.map
                (fun spec -> make_parser_test spec.name spec.program spec.ast)
                test_specs;
         "interpreter"
         >::: List.map
                (fun spec -> make_interpreter_test spec.name spec.program)
                test_specs;
       ]

let () = run_test_tt_main tests
