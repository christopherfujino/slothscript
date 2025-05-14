open Sloth_script

let rec repl () =
  (* %! means flush *)
  Printf.printf "> %!";
  let line = read_line () in
  let ast = Lexing.from_string line |> Parser.prog Lexer.read in
  Ast.stmt_to_str ast |> print_endline;
  repl ()

let () = repl ()
