open OUnit2

type test_spec = {
  name : string;
  program : string;
  ast : string;
  stdout_expect : string;
}

let test_specs =
  [
    { name = "empty program"; program = ""; ast = "()"; stdout_expect = "" };
    {
      name = "num literal";
      program = "11;";
      ast = "((ExprStmt(Num 11)))";
      stdout_expect = "";
    };
    {
      name = "print num";
      program = "print(11);";
      ast = "((ExprStmt(FuncInvoc print((Num 11)))))";
      stdout_expect = "11\n";
    };
    {
      name = "string literal";
      program = "\"Hello\";";
      ast = "((ExprStmt(String Hello)))";
      stdout_expect = "";
    };
    {
      name = "print string";
      program = "print(\"Hello\");";
      ast = "((ExprStmt(FuncInvoc print((String Hello)))))";
      stdout_expect = "Hello\n";
    };
    {
      name = "bool literal";
      program = "true;";
      ast = "((ExprStmt(Bool true)))";
      stdout_expect = "";
    };
    {
      name = "addition";
      program = "1 + 1;";
      ast = "((ExprStmt(Binary Add(Num 1)(Num 1))))";
      stdout_expect = "";
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
      stdout_expect = "";
    };
    {
      name = "func definition";
      program = "func m() {23+19;}";
      ast =
        "((FuncStmt(name m)(parameters())(block((ExprStmt(Binary Add(Num \
         23)(Num 19)))))))";
      stdout_expect = "";
    };
    {
      name = "func invocation";
      program = "func m() {1;2;3;}m();";
      ast =
        "((FuncStmt(name m)(parameters())(block((ExprStmt(Num 1))(ExprStmt(Num \
         2))(ExprStmt(Num 3)))))(ExprStmt(FuncInvoc m())))";
      stdout_expect = "";
    };
  ]

let rec indent buf n =
  if n = 0 then Buffer.contents buf
  else (
    Buffer.add_char buf ' ';
    (indent [@tailrec]) buf (n - 1))

let tests =
  let printer s = Printf.sprintf "\"%s\"" s in
  let make_parser_test spec =
    let open Compiler in
    spec.name >:: fun _ ->
    let prog = Main.parse spec.program in
    let f formatter left_right_tuple =
      let left, right = left_right_tuple in
      let rec diff_finder i left' right' =
        try
          let lchar = String.get left' i in
          let rchar = String.get right' i in
          if lchar != rchar then i
          else (diff_finder [@tailrec]) (i + 1) left' right'
          (* Catch index out of bounds *)
        with Invalid_argument _ -> i
      in
      let i = diff_finder 0 left right in
      let right_len = String.length right in
      (* TODO write a recursive word boundary finder *)
      let trunc_len = min (i + 6) right_len in
      let right_trunc = String.sub right 0 trunc_len in
      Format.fprintf formatter "First diff at %d\n\n%s\n%s^" i right_trunc
        (indent (Buffer.create i) i)
    in
    assert_equal ~pp_diff:f ~printer spec.ast
      (Optimizer.prog_to_str prog)
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
