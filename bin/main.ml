let rec repl () =
  (* %! means flush *)
  Printf.printf "> %!";
  let line =
    try read_line ()
    with End_of_file ->
      Printf.printf "\n";
      exit 0
  in
  let stmt = Compiler.Main.parse_line line in
  let open Interpreter in
  Interpret.interpret_stmt (Context.make_ctx (module Sloth_stdlib.Prod)) stmt;
  (repl [@tailcall]) ()

let rec interpret () =
  let line =
    try read_line ()
    with End_of_file ->
      Printf.printf "\n";
      exit 0
  in
  let stmt = Compiler.Main.parse_line line in
  let open Interpreter in
  Interpret.interpret_stmt (Context.make_ctx (module Sloth_stdlib.Prod)) stmt;
  (interpret [@tailcall]) ()

let () = if Unix.isatty Unix.stdin then repl () else interpret ()
