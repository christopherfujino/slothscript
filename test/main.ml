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
  let pretty_ast = Printer.sexp_formatter spec.ast in
  (* Parser *)
  let env =
    Compiler.Environment.create spec.program |> Compiler.Environment.populate
  in
  let _, prog = Main.parse env spec.program in

  let proc_spec = Interpreter.Mock_process.spec_of_string spec.proc_spec in

  (* Interpreter *)
  let lib = Interpreter.Native.make_test proc_spec in
  let module Lib = (val lib) in
  let ctx =
    Interpreter.Globals.make_globals
      (module Lib)
      spec.program "/parent/unit_test.sloth"
  in
  let _ = Interpreter.Interpret.interpret_prog ctx prog in
  let forward_buffer = List.rev !Lib.stdout_buffer in
  let catted_output_opt =
    List.fold_left forward_buffer
      ~f:(fun acc cur ->
        Some (match acc with None -> cur | Some acc -> acc ^ cur))
      ~init:None
  in
  (* Is STDOUT correct? *)
  (match catted_output_opt with
  | None -> assert_equal ~printer spec.stdout_expect ""
  | Some s ->
      assert_equal ~printer spec.stdout_expect (String.strip s)
        ~msg:"STDOUT did not meet expectations");

  (* Is AST pretty? *)
  assert_equal ~pp_diff ~printer spec.ast
    (Optimizer.prog_to_str prog |> Printer.sexp_formatter);

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
    assert_failure msg)

let make_failing_test spec =
  let open Compiler in
  spec.name >:: fun _ ->
  let handle_failure actual_msg actual_name : unit =
    match spec.failure with
    | None -> assert_failure @@ Printf.sprintf "%s: %s" actual_name actual_msg
    | Some expectation ->
        if String.is_substring actual_msg ~substring:expectation then ()
        else
          assert_failure
          @@ Printf.sprintf
               "Received unexpected error\n\n\
                Expected: \"%s\"\n\n\
                Received:\n\n\
                %s"
               expectation actual_msg
  in
  (* Parser *)
  try
    let env =
      Compiler.Environment.create spec.program |> Compiler.Environment.populate
    in
    let _, prog = Main.parse env spec.program in
    assert_equal ~pp_diff ~printer spec.ast (Optimizer.prog_to_str prog);

    (* Interpreter *)
    let proc_spec = Interpreter.Mock_process.spec_of_string spec.proc_spec in
    let lib = Interpreter.Native.make_test proc_spec in
    let module Lib = (val lib) in
    let ctx =
      Interpreter.Globals.make_globals
        (module Lib)
        spec.program "unit_test.sloth"
    in
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
  | Sloth_common.Common.CompileError msg ->
      handle_failure msg "CompileError"
  | Sloth_common.Common.RuntimeError msg ->
      handle_failure msg "RuntimeError"
  | Sloth_common.Common.ParseError msg ->
      handle_failure msg "ParseError"

let tests =
  "slothscript"
  >::: [
         "green" >::: List.map ~f:make_test (Specs.green ());
         "red" >::: List.map ~f:make_failing_test Specs.red;
         "unit" >::: Unit_tests.get ();
       ]

let () = run_test_tt_main tests
