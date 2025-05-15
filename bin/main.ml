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
  let ast = Compiler.parse_line line in
  Ast.stmt_to_str ast |> print_endline;
  repl ()

let rec interpret () =
  let line =
    try read_line ()
    with End_of_file ->
      Printf.printf "\n";
      exit 0
  in
  let ast = Compiler.parse_line line in
  Ast.stmt_to_str ast |> print_endline;
  interpret ()

let () = if Unix.isatty Unix.stdin then repl () else interpret ()
