let wrap_interpret ctx prog =
  let open Interpreter in
  try
    Interpret.interpret_prog ctx prog
  with Common.Failure msg ->
    Printf.fprintf stderr "Runtime Exception\n%s\n" msg;
    exit 1

let repl () =
  let ctx =
    Interpreter.Context.make_ctx (module Interpreter.Sloth_stdlib.Prod)
  in
  let env = Compiler.Environment.create () |> Compiler.Stdlib_stubs.populate in
  let rec repl_inner ctx env =
    (* %! means flush *)
    Printf.printf "> %!";
    let line =
      try read_line ()
      with End_of_file ->
        Printf.printf "\n";
        exit 0
    in
    let env, prog = Compiler.Main.parse env line in
    let open Interpreter in
    let ctx, v = wrap_interpret ctx prog in
    Runtime.to_s v |> print_endline;
    (repl_inner [@tailcall]) ctx env
  in
  repl_inner ctx env

let interpreter () =
  let ctx =
    Interpreter.Context.make_ctx (module Interpreter.Sloth_stdlib.Prod)
  in
  let env = Compiler.Environment.create () |> Compiler.Stdlib_stubs.populate in
  let rec read_all buf =
    let cur_line_opt = try Some (read_line ()) with End_of_file -> None in
    match cur_line_opt with
    | None -> Buffer.contents buf
    | Some cur_line ->
        Buffer.add_string buf cur_line;
        Buffer.add_char buf '\n';
        (read_all [@tailcall]) buf
  in
  let program = read_all (Buffer.create 256) in
  let _, ir = Compiler.Main.parse env program in
  let _, _ = wrap_interpret ctx ir in
  ()

let () = if Unix.isatty Unix.stdin then repl () else interpreter ()
