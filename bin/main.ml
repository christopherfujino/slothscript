open Sloth_script

let rec repl () =
  (* %! means flush *)
  Printf.printf "> %!";
  let line =
    try read_line ()
    with End_of_file ->
      Printf.printf "\n";
      exit 0
  in
  let stmt = Compiler.parse_line line in
  Interpreter.interpret_stmt (Interpreter.make_ctx ()) stmt;
  repl ()

let rec interpret () =
  let line =
    try read_line ()
    with End_of_file ->
      Printf.printf "\n";
      exit 0
  in
  let stmt = Compiler.parse_line line in
  Interpreter.interpret_stmt (Interpreter.make_ctx ()) stmt;
  interpret ()

let () = if Unix.isatty Unix.stdin then repl () else interpret ()
