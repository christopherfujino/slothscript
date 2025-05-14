open Sloth_script

let rec expr_to_str expr =
  let open Ast in
  match expr with Num f -> string_of_float f | Bool b -> string_of_bool b

and stmt_to_str stmt =
  let open Ast in
  match stmt with
  | LetStmt (name, expr) ->
      Printf.sprintf "let %s = %s" name (expr_to_str expr)

let rec repl () =
  (* %! means flush *)
  Printf.printf "> %!";
  let line = read_line () in
  let ast = Lexing.from_string line |> Parser.prog Lexer.read in
  stmt_to_str ast |> print_endline;
  repl ()

let () = repl ()
