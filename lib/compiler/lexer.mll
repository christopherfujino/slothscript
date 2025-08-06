(* header *)
{
open Parser

exception SyntaxError of string * Lexing.position

(* TODO pretty print *)
let syntax_error e = raise e

type globalLexerState =
  | NotInterpolating
  | Interpolating

let string_buffer_size = 33

let state = ref NotInterpolating
}

(* identifiers *)
let white = [' ' '\t']+
let num = ('0'|(['1'-'9']['0'-'9']*)) ('.' ['0'-'9']+)?
let letter = ['a'-'z' 'A'-'Z']
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

(* rule and parse are keywords *)
rule read last_token =
  parse
  (* means if `white` matches, call the read rule again and return its
     results--i.e. skip this match *)
  | white { (read [@tailcall]) last_token lexbuf }
  | '\n' {
    (* https://ohama.github.io/ocaml/ocamllex-tutorial/actions/position/ *)
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with
      pos_lnum = lexbuf.lex_curr_p.pos_lnum + 1;
      pos_bol = lexbuf.lex_curr_p.pos_cnum;
    };
    (*
      automatic semicolon insertion
      Inspired by: https://go101.org/article/line-break-rules.html
    *)
    let asi () =
      let token = SEMICOLON lexbuf.lex_curr_p in
      last_token := Some token;
      token in
    match !last_token with
    | None -> (read [@tailcall]) last_token lexbuf
    | Some t -> (match t with
        | ID _ -> asi ()
        | RPAREN _ -> asi ()
        | RCURLY _ -> asi ()
        | RBRACKET _ -> asi ()
        | NUM _ -> asi ()
        | STRING_FULL _ -> asi ()
        | STRING_END _ -> asi ()
        | _ -> (read [@tailcall]) last_token lexbuf
    )
  }
  | "true" { let token = TRUE lexbuf.lex_curr_p in last_token := Some token; token }
  | "false" { let token = FALSE lexbuf.lex_curr_p in last_token := Some token; token }
  | "null" { let token = NULL lexbuf.lex_curr_p in last_token := Some token; token }
  | "let" { let token = LET lexbuf.lex_curr_p in last_token := Some token; token }
  | "func" { let token = FUNC lexbuf.lex_curr_p in last_token := Some token; token }
  | "if" { let token = IF lexbuf.lex_curr_p in last_token := Some token; token }
  | "else" { let token = ELSE lexbuf.lex_curr_p in last_token := Some token; token }
  | "for" { let token = FOR lexbuf.lex_curr_p in last_token := Some token; token }
  | '+' { let token = PLUS lexbuf.lex_curr_p in last_token := Some token; token }
  | '-' { let token = MINUS lexbuf.lex_curr_p in last_token := Some token; token }
  | '=' { let token = EQUALS lexbuf.lex_curr_p in last_token := Some token; token }
  | '*' { let token = PRODUCT lexbuf.lex_curr_p in last_token := Some token; token }
  | '/' { let token = DIVIDE lexbuf.lex_curr_p in last_token := Some token; token }
  | ';' {
    let parse_semicolon () =
      let token = SEMICOLON lexbuf.lex_curr_p in
      last_token := Some token;
      token
    in
    match !last_token with
    | None -> (* TODO should we ignore leading semicolon? *) parse_semicolon ()
    | Some t -> (match t with
        (* Allow no-op repeat semicolons *)
        | SEMICOLON _ -> (read [@tailcall]) last_token lexbuf
        | _ -> parse_semicolon ()
    )
  }
  | ':' { let token = COLON lexbuf.lex_curr_p in last_token := Some token; token }
  | '"' {
    let token = read_string (Buffer.create string_buffer_size) lexbuf.lex_curr_p lexbuf in
    last_token := Some token;
    token
  }
  | '{' { let token = LCURLY lexbuf.lex_curr_p in last_token := Some token; token}
  | '}' {
    let token = (match !state with
    | NotInterpolating -> RCURLY lexbuf.lex_curr_p
    | Interpolating -> (read_string (Buffer.create string_buffer_size) lexbuf.lex_curr_p lexbuf)) in
    last_token := Some token;
    token
  }
  | '(' { let token = LPAREN lexbuf.lex_curr_p in last_token := Some token; token}
  | ')' { let token = RPAREN lexbuf.lex_curr_p in last_token := Some token; token}
  | ',' { let token = COMMA lexbuf.lex_curr_p in last_token := Some token; token}
  | '<' { let token = LESS lexbuf.lex_curr_p in last_token := Some token; token}
  | "<=" { let token = LEQ lexbuf.lex_curr_p in last_token := Some token; token}
  | '[' { let token = LBRACKET lexbuf.lex_curr_p in last_token := Some token; token}
  | ']' { let token = RBRACKET lexbuf.lex_curr_p in last_token := Some token; token}
  (* Lexing.lexeme means return the string that matched the pattern *)
  | id { let token = ID (Lexing.lexeme lexbuf, lexbuf.lex_curr_p) in last_token := Some token; token }
  | num { let token = NUM (float_of_string (Lexing.lexeme lexbuf), lexbuf.lex_curr_p) in last_token := Some token; token }
  | _ { syntax_error (SyntaxError (Lexing.lexeme lexbuf, lexbuf.lex_curr_p))}
  (* Here `eof` is a special regex built into ocamllex *)
  | eof {
    (* Note: if needed, a semicolon will be inserted later in filtering
       between lexing & parsing *)
    EOF lexbuf.lex_curr_p
  }

and read_string buf pos =
  (* TODO implement escapes *)
  parse
  | '"' {
    match !state with
    | NotInterpolating -> STRING_FULL (Buffer.contents buf, pos)
    | Interpolating -> (
      state := NotInterpolating;
      STRING_END (Buffer.contents buf, pos)
    )
  }
  | "${" {
    match !state with
    | NotInterpolating -> (
      state := Interpolating;
      STRING_START (Buffer.contents buf, pos)
    )
    | Interpolating -> STRING_MIDDLE (Buffer.contents buf, pos)
  }
  | [^ '"' '$']+ {
    let chunk = (Lexing.lexeme lexbuf) in
    Buffer.add_string buf chunk;
      (read_string[@tailcall]) buf pos lexbuf }
