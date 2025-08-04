open Core

(* insert semicolon before EOF *)
let make_lex_filter () =
  let r = ref None in
  let last_token = ref None in
  let should_spit_eof = ref false in
  (* insert semicolon before EOF *)
  fun buf ->
    let open Parser in
    if !should_spit_eof then EOF
    else
      let token = Lexer.read r buf in
      match token with
      | EOF -> (
          match !last_token with
          | None -> EOF
          | Some last_token -> (
              match last_token with
              | SEMICOLON -> EOF
              | _ ->
                  should_spit_eof := true;
                  SEMICOLON))
      | _ ->
          last_token := Some token;
          token

let parse env line =
  let lex_filter = make_lex_filter () in
  let decls =
    try
      let lexbuf = Lexing.from_string line in
      Parser.prog lex_filter lexbuf
    with Parser.Error i ->
      let msg = Printf.sprintf "Parser error (%d)" i in
      (* TODO Interpolate lexer position *)
      raise (Common.ParserFailure msg)
  in
  Optimizer.optimize_prog env decls

(* TODO figure out how to derive this *)
let to_s token =
  let open Parser in
  match token with
  | COLON -> "COLON"
  | SEMICOLON -> "SEMICOLON"
  | TRUE _ -> "TRUE"
  | RPAREN -> "RPAREN"
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
  | NUM (n, _) -> Printf.sprintf "NUM(%f)" n
  | ID s -> Printf.sprintf "ID(%s)" s

let debug buf prev =
  let lex_filter = make_lex_filter () in
  let rec inner buf idx r prev =
    let cur = lex_filter buf in
    if phys_equal cur Parser.EOF then (
      let prev = List.rev prev in
      print_endline "[Start]";
      List.iter prev ~f:(fun t -> to_s t |> print_endline);
      ())
    else (inner [@tailcall]) buf (idx + 1) r (cur :: prev)
  in
  inner buf 0 (ref None) prev
