open Core

let parse env line =
  let filter, lexbuf = Lexer.bootstrap line in
  let decls =
    try Parser.prog filter lexbuf with
    | Parser.Error i ->
        let pos =
          Sloth_common.Position.t_of_lexing_position lexbuf.lex_start_p
        in
        let pos_str = Sloth_common.Position.string_of_t pos in
        let msg = Parser_errors.message i |> String.strip in
        let line_no = pos.pos_lnum in
        let msg =
          match msg with
          | "<YOUR SYNTAX ERROR MESSAGE HERE>" ->
              Printf.sprintf "[%s] Parser error (code #%d)\n\n%s" pos_str
                i
                (Sloth_common.Position.summarize pos line)
          | _ ->
              Printf.sprintf "[%s] Parser error (code #%d):\n\n%s\n%s" pos_str
                line_no
                (Sloth_common.Position.summarize pos line)
                msg
        in

        raise (Sloth_common.Common.ParseError msg)
    | Lexer.SyntaxError (msg, pos) ->
        let pos = Sloth_common.Position.t_of_lexing_position pos in
        let msg =
          Printf.sprintf "Lexer error: Unknown token `%s`\n\n%s\n" msg
            (Sloth_common.Position.summarize pos line)
        in
        raise (Sloth_common.Common.LexerError msg)
  in
  Optimizer.optimize_prog env decls

(* TODO figure out how to derive this *)
let to_s token =
  let open Parser in
  match token with
  | BANG _ -> "BANG"
  | LEFT_ARROW _ -> "LEFT_ARROW"
  | RIGHT_ARROW _ -> "RIGHT_ARROW"
  | NOT _ -> "NOT"
  | DOT _ -> "DOT"
  | PIPE _ -> "PIPE"
  | COLON _ -> "COLON"
  | SEMICOLON _ -> "SEMICOLON"
  | TRUE _ -> "TRUE"
  | RPAREN _ -> "RPAREN"
  | RCURLY _ -> "RCURLY"
  | RBRACKET _ -> "RBRACKET"
  | PRODUCT _ -> "PRODUCT"
  | PLUS _ -> "PLUS"
  | NULL _ -> "NULL"
  | MINUS _ -> "MINUS"
  | LPAREN _ -> "LPAREN"
  | LET _ -> "LET"
  | WITH _ -> "WITH"
  | RETURN _ -> "RETURN"
  | BREAK _ -> "BREAK"
  | CONTINUE _ -> "CONTINUE"
  | LESS _ -> "LESS"
  | GREATER _ -> "GREATER"
  | DOUBLE_EQUALS _ -> "DOUBLE_EQUALS"
  | NOT_EQUALS _ -> "NOT_EQUALS"
  | LEQ _ -> "LEQ"
  | GEQ _ -> "GEQ"
  | LCURLY _ -> "LCURLY"
  | LBRACKET _ -> "LBRACKET"
  | IN _ -> "IN"
  | DO _ -> "DO"
  | IF _ -> "IF"
  | FUNC _ -> "FUNC"
  | FOR _ -> "FOR"
  | FALSE _ -> "FALSE"
  | EQUALS _ -> "EQUALS"
  | EOF _ -> "EOF"
  | ELSE _ -> "ELSE"
  | DIVIDE _ -> "DIVIDE"
  | COMMA _ -> "COMMA"
  | STRING_START (s, _) -> Printf.sprintf "STRING_START(%s)" s
  | STRING_MIDDLE (s, _) -> Printf.sprintf "STRING_MIDDLE(%s)" s
  | STRING_FULL (s, _) -> Printf.sprintf "STRING_FULL(%s)" s
  | STRING_END (s, _) -> Printf.sprintf "STRING_END(%s)" s
  | NUM (n, _) -> Printf.sprintf "NUM(%f)" n
  | ID (s, _) -> Printf.sprintf "ID(%s)" s
  | CONTEXT_ID (s, _) -> Printf.sprintf "CONTEXT_ID(%s)" s
  | PROTOTYPE (s, _) -> Printf.sprintf "PROTOTYPE(%s)" s
  | COMMENT _ -> failwith "Unreachable"

let debug buf prev =
  let lex_filter = Lexer.make_lex_filter () in
  let rec inner buf idx r prev =
    let cur = lex_filter buf in
    match cur with
    | Parser.EOF _ ->
        let prev = List.rev prev in
        print_endline "[Start]";
        List.iter prev ~f:(fun t -> to_s t |> print_endline);
        ()
    | _ -> (inner [@tailcall]) buf (idx + 1) r (cur :: prev)
  in
  inner buf 0 (ref None) prev
