open OUnit2

type test_spec = {
  name : string;
  program : string;
  ast : string;
  stdout_expect : string;
  failure : failure_t option;
}

and failure_t = Optimizer_error | Runtime_error

let test_specs =
  [
    {
      name = "empty program";
      program = "";
      ast = "()";
      stdout_expect = "";
      failure = None;
    };
    {
      name = "num literal";
      program = "11;";
      ast = "((ExprStmt(Num 11)))";
      stdout_expect = "";
      failure = None;
    };
    {
      name = "print num";
      program = "print(11);";
      ast = "((ExprStmt(FuncInvoc print((Num 11)))))";
      stdout_expect = "11\n";
      failure = None;
    };
    {
      name = "string literal";
      program = "\"Hello\";";
      ast = "((ExprStmt(String Hello)))";
      stdout_expect = "";
      failure = None;
    };
    {
      name = "print string";
      program = "print(\"Hello\");";
      ast = "((ExprStmt(FuncInvoc print((String Hello)))))";
      stdout_expect = "Hello\n";
      failure = None;
    };
    {
      name = "bool literal";
      program = "true;";
      ast = "((ExprStmt(Bool true)))";
      stdout_expect = "";
      failure = None;
    };
    {
      name = "addition";
      program = "1 + 1;";
      ast = "((ExprStmt(Binary Add(Num 1)(Num 1))))";
      stdout_expect = "";
      failure = None;
    };
    {
      name = "assignment";
      program = "let x = 1 + 1;";
      ast = "((LetStmt x(Binary Add(Num 1)(Num 1))))";
      stdout_expect = "";
      failure = None;
    };
    {
      name = "var reference";
      program = "let x = 1 + 1;\nprint(x);";
      ast =
        "((LetStmt x(Binary Add(Num 1)(Num 1)))(ExprStmt(FuncInvoc \
         print((IdRef x)))))";
      stdout_expect = "2\n";
      failure = None;
    };
    {
      name = "re-assignment";
      program = "let x = 0;x = 1;print(x);";
      ast =
        "((LetStmt x(Num 0))(AssignStmt x(Num 1))(ExprStmt(FuncInvoc \
         print((IdRef x)))))";
      stdout_expect = "1\n";
      failure = None;
    };
    {
      name = "func definition";
      program = "func m() {}";
      ast = "((FuncStmt(name m)(parameters())(block())))";
      stdout_expect = "";
      failure = None;
    };
    {
      name = "func invocation";
      program = "func m() {1;2;3;}print(m());";
      ast =
        "((FuncStmt(name m)(parameters())(block((ExprStmt(Num 1))(ExprStmt(Num \
         2))(ExprStmt(Num 3)))))(ExprStmt(FuncInvoc print((FuncInvoc m())))))";
      stdout_expect = "3\n";
      failure = None;
    };
    {
      name = "nested functions";
      program = "func f1() {let x = 1;func f2() {print(x);}f2();}f1();";
      ast =
        "((FuncStmt(name f1)(parameters())(block((LetStmt x(Num \
         1))(FuncStmt(name f2)(parameters())(block((ExprStmt(FuncInvoc \
         print((IdRef x)))))))(ExprStmt(FuncInvoc f2())))))(ExprStmt(FuncInvoc \
         f1())))";
      stdout_expect = "1\n";
      failure = None;
    };
    {
      name = "lexical scope";
      program = "let x=1;func f() {print(x);}func g() {let x=2;f();}g();";
      ast =
        "((LetStmt x(Num 1))(FuncStmt(name \
         f)(parameters())(block((ExprStmt(FuncInvoc print((IdRef \
         x)))))))(FuncStmt(name g)(parameters())(block((LetStmt x(Num \
         2))(ExprStmt(FuncInvoc f())))))(ExprStmt(FuncInvoc g())))";
      stdout_expect = "1\n";
      failure = None;
    };
  ]

(* Throw specific runtime exceptions, and assert on them here. *)
let interpreter_failure_specs =
  [
    {
      name = "vars & funcs share namespace";
      program = "let x=1;func x(){}";
      ast = "((LetStmt x(Num 1))(FuncStmt(name x)(parameters())(block())))";
      stdout_expect = "";
      failure = Some Runtime_error;
    };
    (*
    {
      name = "foo";
      program = "let x = 0;func f() {print(x);}x = 1;f();";
      ast = "";
      stdout_expect = "1\n";
    }
    *)
    {
      (* Although this works in JS, Python, and Perl, this seems like a mistake *)
      (* Go does not allow it. *)
      name = "closures cannot capture future vars";
      program = "func closure() {print(x);}let x = 1;closure();";
      ast =
        "((FuncStmt(name closure)(parameters())(block((ExprStmt(FuncInvoc \
         print((IdRef x)))))))(LetStmt x(Num 1))(ExprStmt(FuncInvoc \
         closure())))";
      stdout_expect = "";
      failure = Some Optimizer_error;
    };
  ]

let rec indent buf n =
  if n = 0 then Buffer.contents buf
  else (
    Buffer.add_char buf ' ';
    (indent [@tailrec]) buf (n - 1))

let printer s = Printf.sprintf "\"%s\"" s

let pp_diff formatter left_right_tuple =
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

let tests =
  let make_test spec =
    let open Compiler in
    spec.name >:: fun _ ->
    (* Parser *)
    let prog = Main.parse spec.program in
    assert_equal ~pp_diff ~printer spec.ast (Optimizer.prog_to_str prog);

    (* Interpreter *)
    let module Lib = Interpreter.Sloth_stdlib.Make_test () in
    let ctx = Interpreter.Context.make_ctx (module Lib) in
    let _, _ = Interpreter.Interpret.interpret_prog ctx prog in
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
  let make_failing_test spec =
    let open Compiler in
    spec.name >:: fun _ ->
    (* Parser *)
    try
      let prog = Main.parse spec.program in
      assert_equal ~pp_diff ~printer spec.ast (Optimizer.prog_to_str prog);

      (* Interpreter *)
      let module Lib = Interpreter.Sloth_stdlib.Make_test () in
      let ctx = Interpreter.Context.make_ctx (module Lib) in
      let _, _ = Interpreter.Interpret.interpret_prog ctx prog in
      let cb =
       fun acc cur ->
        match acc with None -> Some cur | Some acc -> Some (acc ^ ", " ^ cur)
      in
      let buf_s = List.fold_left cb None !Lib.stdout_buffer |> Option.get in
      let msg =
        Printf.sprintf
          "test did not throw a runtime error as expected\n\
           stdout_buffer is = %s"
          buf_s
      in
      assert_failure msg
    with
    | Optimizer.Failure msg -> (
        match spec.failure with
        | None ->
            Printf.sprintf "Expected no failure, but got Optimizer.Failure (%s)"
              msg
            |> assert_failure
        | Some expectation -> (
            match expectation with
            | Optimizer_error -> ()
            | Runtime_error ->
                Printf.sprintf
                  "Expected a Runtime_error, but got a Optimizer.Failure: %s"
                  msg
                |> assert_failure))
    | Interpreter.Common.Failure msg -> (
        match spec.failure with
        | None ->
            Printf.sprintf
              "Expected no failure, but got Interpreter.Common.Failure (%s)" msg
            |> assert_failure
        | Some expectation -> (
            match expectation with
            | Optimizer_error ->
                Printf.sprintf
                  "Expected an Opimize_error, but got a \
                   Interpreter.Common.Failure (%s)"
                  msg
                |> assert_failure
            | Runtime_error -> ()))
  in
  "slothscript"
  >::: [
         "green" >::: List.map make_test test_specs;
         "red" >::: List.map make_failing_test interpreter_failure_specs;
       ]

let () = run_test_tt_main tests
