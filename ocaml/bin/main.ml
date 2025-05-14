open Sloth_script

let rec repl () =
  (* %! means flush *)
  Printf.printf "> %!";
  let line = read_line () in
  let ast = Compiler.parse_line line in
  Ast.stmt_to_str ast |> print_endline;
  repl ()

let () = repl ()
