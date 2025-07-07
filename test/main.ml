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
  let rec diff_finder i left' right' =
    try
      let lchar = String.get left' i in
      let rchar = String.get right' i in
      if not (Char.( = ) lchar rchar) then i
      else diff_finder (i + 1) left' right'
      (* Catch index out of bounds *)
    with Invalid_argument _ -> i
  in
  let i = diff_finder 0 left right in
  let right_len = String.length right in
  (* TODO write a recursive word boundary finder *)
  let trunc_len = min (i + 6) right_len in
  let right_trunc = String.sub right ~pos:0 ~len:trunc_len in
  Format.fprintf formatter "First diff at %d\n\n%s\n%s^" i right_trunc
    (indent (Buffer.create i) i)

let make_test spec =
  let open Compiler in
  spec.name >:: fun _ ->
  (* Is AST pretty? *)
  (* TODO un-comment this
  let pretty_ast = Printer.sexp_formatter spec.ast in
  (if not (String.equal pretty_ast spec.ast) then
     let msg =
       Printf.sprintf "Un-pretty AST for %s\n\nExpected:\n%s" spec.name
         pretty_ast
     in
     assert_failure msg);
     *)
  (* Parser *)
  let env = Compiler.Environment.create () |> Stdlib_stubs.populate in
  let _, prog = Main.parse env spec.program in
  assert_equal ~pp_diff ~printer
    (spec.ast |> Printer.sexp_formatter (* TODO delete this hack *))
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
  | Optimizer.Failure msg -> (
      match spec.failure with
      | None ->
          Printf.sprintf "Expected no failure, but got Optimizer.Failure (%s)"
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
       ]

let () = run_test_tt_main tests
