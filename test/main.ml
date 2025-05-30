open OUnit2

type test_spec = {
  name : string;
  program : string;
  ast : string;
  stdout_expect : string;
}

let test_specs =
  [
    {
      name = "empty program";
      program = "";
      ast = "()";
      stdout_expect = "";
    };
    {
      name = "num literal";
      program = "11;";
      ast = "((ExprStmt(Num 11)))";
      stdout_expect = "11\n";
    };
    {
      name = "string literal";
      program = "\"Hello\";";
      ast = "((ExprStmt(String Hello)))";
      stdout_expect = "\"Hello\"\n";
    };
    {
      name = "bool literal";
      program = "true;";
      ast = "((ExprStmt(Bool true)))";
      stdout_expect = "true\n";
    };
    {
      name = "addition";
      program = "1 + 1;";
      ast = "((ExprStmt(Binary Add(Num 1)(Num 1))))";
      stdout_expect = "2\n";
    };
    {
      name = "assignment";
      program = "let x = 1 + 1;";
      ast = "((LetStmt x(Binary Add(Num 1)(Num 1))))";
      stdout_expect = "";
    };
    {
      name = "var reference";
      program = "let x = 1 + 1;\nx;";
      ast = "((LetStmt x(Binary Add(Num 1)(Num 1)))(ExprStmt(IdRef x)))";
      stdout_expect = "2\n";
    };
    {
      name = "func definition";
      program = "func m() {23+19;}";
      ast = "((FuncStmt((name m)(parameters())(block((ExprStmt(Binary Add(Num 23)(Num 19))))))))";
      stdout_expect = "";
    };
  ]

let tests =
  let printer s = Printf.sprintf "\"%s\"" s in
  let make_parser_test spec =
    let open Compiler in
    spec.name >:: fun _ ->
    let prog = Main.parse spec.program in
    assert_equal ~printer spec.ast (Ast.prog_to_str prog)
  in
  let make_interpreter_test spec =
    let open Compiler in
    spec.name >:: fun _ ->
    let stmt = Main.parse spec.program in
    let module Lib = Interpreter.Sloth_stdlib.Make_test () in
    let ctx = Interpreter.Context.make_ctx (module Lib) in
    Interpreter.Interpret.interpret_prog ctx stmt;
    let catted_output_opt =
      List.fold_left
        (fun acc cur ->
          Some
            (match acc with
            | None -> cur
            | Some acc -> Printf.sprintf "%s\n%s" acc cur))
        None !Lib.stdout_buffer
    in
    match catted_output_opt with
    | None -> assert_equal ~printer spec.stdout_expect ""
    | Some s -> assert_equal ~printer spec.stdout_expect s
  in
  let parser_tests =
    "parser" >::: List.map (fun spec -> make_parser_test spec) test_specs
  in
  let interpreter_tests =
    "interpreter"
    >::: List.map (fun spec -> make_interpreter_test spec) test_specs
  in
  "slothscript" >::: [ parser_tests; interpreter_tests ]

let () = run_test_tt_main tests
