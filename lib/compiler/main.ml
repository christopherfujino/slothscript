open Core

let parse env line =
  let decls =
    try Lexing.from_string line |> Parser.prog Lexer.read
    with Parser.Error i ->
      let msg = Printf.sprintf "Parser error (%d)" i in
      (* TODO Interpolate lexer position *)
      raise (Common.ParserFailure msg)
  in
  Optimizer.optimize_prog env decls

let to_s token =
  let open Parser in
  match token with
  | COLON -> "COLON"
  | SEMICOLON -> "SEMICOLON"
  | TRUE -> "TRUE"
  | RPAREN -> "TRUE"
  | RCURLY -> "RCURLY"
  | RBRACKET -> "RBRACKET"
  | PRODUCT -> "PRODUCT"
  | PLUS -> "PLUS"
  | NULL -> "NULL"
  | MINUS -> "MINUS"
  | LPAREN -> "LPAREN"
  | LET -> "LET"
  | LESS -> "LESS"
  | LEQ -> "LEQ"
  | LCURLY -> "LCURLY"
  | LBRACKET -> "LBRACKET"
  | IF -> "IF"
  | FUNC -> "FUNC"
  | FOR -> "FOR"
  | FALSE -> "FALSE"
  | EQUALS -> "EQUALS"
  | EOF -> "EOF"
  | ELSE -> "ELSE"
  | DIVIDE -> "DIVIDE"
  | COMMA -> "COMMA"
  | STRING_START s -> Printf.sprintf "STRING_START(%s)" s
  | STRING_MIDDLE s -> Printf.sprintf "STRING_MIDDLE(%s)" s
  | STRING_FULL s -> Printf.sprintf "STRING_FULL(%s)" s
  | STRING_END s -> Printf.sprintf "STRING_END(%s)" s
  | NUM n -> Printf.sprintf "NUM(%f)" n
  | ID s -> Printf.sprintf "ID(%s)" s

(* TODO delete *)
let rec debug buf ?(idx = 0) prev =
  let cur = Lexer.read buf in
  if phys_equal cur Parser.EOF then (
    let prev = List.rev prev in
    print_endline "[Start]";
    List.iter prev ~f:(fun t -> to_s t |> print_endline);
    ())
  else (debug [@tailcall]) buf ~idx:(idx + 1) (Lexer.read buf :: prev)
