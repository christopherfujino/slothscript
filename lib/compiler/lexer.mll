(* header *)
{
open Parser

exception SyntaxError of string

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
      last_token := Some SEMICOLON; SEMICOLON in
    match !last_token with
    | None -> (read [@tailcall]) last_token lexbuf
    | Some t -> (match t with
        | ID _ -> asi ()
        | RPAREN -> asi ()
        | RCURLY -> asi ()
        | RBRACKET -> asi ()
        | NUM _ -> asi ()
        | STRING_FULL _ -> asi ()
        | STRING_END _ -> asi ()
        | _ -> (read [@tailcall]) last_token lexbuf
    )
  }
  | "true" { last_token := Some TRUE; Option.get !last_token }
  | "false" { last_token := Some FALSE; Option.get !last_token }
  | "null" { last_token := Some NULL; Option.get !last_token }
  | "let" { last_token := Some LET; Option.get !last_token }
  | "func" { last_token := Some FUNC; FUNC }
  | "if" { last_token := Some IF; Option.get !last_token }
  | "else" { last_token := Some ELSE; Option.get !last_token }
  | "for" { last_token := Some FOR; Option.get !last_token }
  | '+' { last_token := Some PLUS; Option.get !last_token }
  | '=' { last_token := Some EQUALS; Option.get !last_token }
  | '*' { last_token := Some PRODUCT; Option.get !last_token }
  | '/' { last_token := Some DIVIDE; Option.get !last_token }
  | ';' {
    let parse_semicolon () =
      last_token := Some SEMICOLON;
      SEMICOLON
    in
    match !last_token with
    | None -> parse_semicolon ()
    | Some t -> (match t with
        (* Allow no-op repeat semicolons *)
        | SEMICOLON -> (read [@tailcall]) last_token lexbuf
        | _ -> parse_semicolon ()
    )
  }
  | ':' { last_token := Some COLON; Option.get !last_token }
  | '"' {
    last_token := Some (read_string (Buffer.create string_buffer_size) lexbuf);
    Option.get !last_token
  }
  | '{' { last_token := Some LCURLY; Option.get !last_token }
  | '}' {
    last_token := (match !state with
    | NotInterpolating -> Some RCURLY
    | Interpolating -> Some (read_string (Buffer.create string_buffer_size) lexbuf));
    Option.get !last_token
  }
  | '(' { last_token := Some LPAREN; Option.get !last_token }
  | ')' { last_token := Some RPAREN; Option.get !last_token }
  | ',' { last_token := Some COMMA; Option.get !last_token }
  | '<' { last_token := Some LESS; Option.get !last_token }
  | "<=" { last_token := Some LEQ; Option.get !last_token }
  | '-' { last_token := Some MINUS; Option.get !last_token }
  | '[' { last_token := Some LBRACKET; Option.get !last_token }
  | ']' { last_token := Some RBRACKET; Option.get !last_token }
  (*
  | '.' { DOT }
  *)
  (* Lexing.lexeme means return the string that matched the pattern *)
  | id { last_token := Some (ID (Lexing.lexeme lexbuf)); Option.get !last_token }
  | num { last_token := Some (NUM (float_of_string (Lexing.lexeme lexbuf))); Option.get !last_token }
  | _ { raise (SyntaxError (Lexing.lexeme lexbuf))}
  (* Here `eof` is a special regex built into ocamllex *)
  | eof {
    (* Note: if needed, a semicolon will be inserted later in filtering
       between lexing & parsing *)
    EOF
  }

and read_string buf =
  (* TODO implement escapes *)
  parse
  | '"' {
    match !state with
    | NotInterpolating -> STRING_FULL (Buffer.contents buf)
    | Interpolating -> (
      state := NotInterpolating;
      STRING_END (Buffer.contents buf)
    )
  }
  | "${" {
    match !state with
    | NotInterpolating -> (
      state := Interpolating;
      STRING_START (Buffer.contents buf)
    )
    | Interpolating -> STRING_MIDDLE (Buffer.contents buf)
  }
  | [^ '"' '$']+ {
    let chunk = (Lexing.lexeme lexbuf) in
    Buffer.add_string buf chunk;
      (read_string[@tailcall]) buf lexbuf }
