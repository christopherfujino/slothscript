open OUnit2
open Common
open Core

let rec indent buf n =
  if n = 0 then Buffer.contents buf
  else (
    Buffer.add_char buf ' ';
    (indent [@tailcall]) buf (n - 1))

let printer s = Printf.sprintf "\"%s\"" s

let pp_diff formatter left_right_tuple =
  let left, right = left_right_tuple in
  let rec diff_finder i i_of_cur_line left' right' =
    try
      let lchar = String.get left' i in
      let rchar = String.get right' i in
      if not (Char.( = ) lchar rchar) then (i, i_of_cur_line)
      else if Char.( = ) lchar '\n' then diff_finder (i + 1) 0 left' right'
      else diff_finder (i + 1) (i_of_cur_line + 1) left' right'
      (* Catch index out of bounds *)
    with Invalid_argument _ -> (i, i_of_cur_line)
  in
  let i, i_cur = diff_finder 0 0 left right in
  let right_len = String.length right in
  (* TODO write a recursive word boundary finder *)
  let trunc_len = min (i + 6) right_len in
  let right_trunc = String.sub right ~pos:0 ~len:trunc_len in
  Format.fprintf formatter "First diff at %d\n\n%s\n%s^" i right_trunc
    (indent (Buffer.create i_cur) i_cur)

let make_test spec =
  let open Compiler in
  spec.name >:: fun _ ->
  (* Is AST pretty? *)
  let pretty_ast = Printer.sexp_formatter spec.ast in
  if not (String.equal pretty_ast spec.ast) then (
    let buf = Buffer.create 256 in
    let formatter = Format.formatter_of_buffer buf in
    pp_diff formatter (pretty_ast, spec.ast);
    (* Flush *)
    Format.pp_print_newline formatter ();
    let msg = Buffer.contents buf in
    let msg =
      Printf.sprintf
        "Un-pretty AST for %s\n\nExpected:\n%s\n\nShould be:\n%s\n\n%s"
        spec.name spec.ast pretty_ast msg
    in
    assert_failure msg);
  (* Parser *)
  let env = Compiler.Environment.create () |> Stdlib_stubs.populate in
  let _, prog = Main.parse env spec.program in
  assert_equal ~pp_diff ~printer spec.ast
    (Optimizer.prog_to_str prog |> Printer.sexp_formatter);

  (* Interpreter *)
  let module Lib = Interpreter.Sloth_stdlib.Make_test () in
  let ctx = Interpreter.Context.make_ctx (module Lib) in
  let _ = Interpreter.Interpret.interpret_prog ctx prog in
  let forward_buffer = List.rev !Lib.stdout_buffer in
  let catted_output_opt =
    List.fold_left forward_buffer
      ~f:(fun acc cur ->
        Some (match acc with None -> cur | Some acc -> acc ^ cur))
      ~init:None
  in
  match catted_output_opt with
  | None -> assert_equal ~printer spec.stdout_expect ""
  | Some s -> assert_equal ~printer spec.stdout_expect (String.strip s)

let make_failing_test spec =
  let open Compiler in
  spec.name >:: fun _ ->
  (* Parser *)
  try
    let env = Compiler.Environment.create () |> Stdlib_stubs.populate in
    let _, prog = Main.parse env spec.program in
    assert_equal ~pp_diff ~printer spec.ast (Optimizer.prog_to_str prog);

    (* Interpreter *)
    let module Lib = Interpreter.Sloth_stdlib.Make_test () in
    let ctx = Interpreter.Context.make_ctx (module Lib) in
    let _ = Interpreter.Interpret.interpret_prog ctx prog in
    let cb =
     fun acc cur ->
      match acc with None -> Some cur | Some acc -> Some (acc ^ ", " ^ cur)
    in
    let buf_s =
      List.fold_left ~f:cb ~init:None !Lib.stdout_buffer |> Option.value_exn
    in
    let msg =
      Printf.sprintf
        "test did not throw a runtime error as expected\nstdout_buffer is = %s"
        buf_s
    in
    assert_failure msg
  with
  | Compiler.Lexer.SyntaxError (err, pos) -> (
      match spec.failure with
      | Some expectation -> (
          match expectation with
          | Scanner_error -> ()
          | _ ->
              let open Sloth_common.Position in
              Printf.sprintf
                "[%s] Expected %s but got Compiler.Lexer.SyntaxError(%s)"
                (t_of_lexing_position pos |> string_of_t)
                (string_of_failure expectation)
                err
              |> failwith)
      | None ->
          Printf.sprintf
            "Expected no failure, but got Compiler.Lexer.SyntaxError(%s)" err
          |> failwith)
  | Optimizer.Failure msg -> (
      match spec.failure with
      | None ->
          Printf.sprintf "Expected no failure, but got Optimizer.Failure(%s)"
            msg
          |> assert_failure
      | Some expectation -> (
          match expectation with
          | Optimizer_error -> ()
          | _ ->
              Printf.sprintf
                "Got an Optimizer.Failure but expected something else: %s" msg
              |> assert_failure))
  | Interpreter.Common.Failure msg -> (
      match spec.failure with
      | None ->
          Printf.sprintf
            "Expected no failure, but got Interpreter.Common.Failure (%s)" msg
          |> assert_failure
      | Some expectation -> (
          match expectation with
          | Runtime_error -> ()
          | _ ->
              Printf.sprintf
                "Got an Interpreter.Common.Failure but expected something else \
                 (%s)"
                msg
              |> assert_failure))
  | Compiler.Common.ParserFailure msg -> (
      match spec.failure with
      | None ->
          Printf.sprintf
            "Expected no failure but got Compiler.Common.ParserFailure (%s)" msg
          |> assert_failure
      | Some expectation -> (
          match expectation with
          | Parser_error -> ()
          | _ ->
              Printf.sprintf
                "Got a Compiler.Common.ParserFailure (%s) but expected a \
                 different error"
                msg
              |> assert_failure))

let tests =
  "slothscript"
  >::: [
         "green" >::: List.map ~f:make_test (Specs.green ());
         "red" >::: List.map ~f:make_failing_test Specs.red;
         "unit" >::: Unit_tests.get ();
       ]

let () = run_test_tt_main tests
