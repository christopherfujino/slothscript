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
}

(* identifiers *)
let white = [' ' '\t']+
let num = ('0'|(['1'-'9']['0'-'9']*)) ('.' ['0'-'9']+)?
let letter = ['a'-'z' 'A'-'Z']

(* IDs cannot start with a capital letter *)
let id = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let prototype = ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let context_id = '$' ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

(* rule and parse are keywords *)
rule private_read last_token state =
  parse
  (* means if `white` matches, call the read rule again and return its
     results--i.e. skip this match *)
  | white { (private_read [@tailcall]) last_token state lexbuf }
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
    | None -> (private_read [@tailcall]) last_token state lexbuf
    | Some t -> (match t with
        | ID _ -> asi ()
        | CONTEXT_ID _ -> asi ()
        | RPAREN _ -> asi ()
        | RCURLY _ -> asi ()
        | RBRACKET _ -> asi ()
        | NUM _ -> asi ()
        | STRING_FULL _ -> asi ()
        | STRING_END _ -> asi ()
        | NULL _ -> asi ()
        | RETURN _ -> asi ()
        | BREAK _ -> asi ()
        | CONTINUE _ -> asi ()
        | TRUE _ -> asi ()
        | FALSE _ -> asi ()
        (* ! is a postfix operator *)
        | BANG _ -> asi ()
        | _ -> (private_read [@tailcall]) last_token state lexbuf
    )
  }
  (* TODO remove copy pasta setting last_token, we do that in the filter *)
  (* Literals *)
  | "true" { let token = TRUE lexbuf.lex_curr_p in last_token := Some token; token }
  | "false" { let token = FALSE lexbuf.lex_curr_p in last_token := Some token; token }
  | "null" { let token = NULL lexbuf.lex_curr_p in last_token := Some token; token }

  (* Keywords *)
  | "let" { let token = LET lexbuf.lex_curr_p in last_token := Some token; token }
  | "func" { let token = FUNC lexbuf.lex_curr_p in last_token := Some token; token }
  | "if" { let token = IF lexbuf.lex_curr_p in last_token := Some token; token }
  | "else" { let token = ELSE lexbuf.lex_curr_p in last_token := Some token; token }
  | "in" { let token = IN lexbuf.lex_curr_p in last_token := Some token; token }
  | "do" { let token = DO lexbuf.lex_curr_p in last_token := Some token; token }
  | "for" { let token = FOR lexbuf.lex_curr_p in last_token := Some token; token }
  | "with" { let token = WITH lexbuf.lex_curr_p in last_token := Some token; token }
  | "return" { let token = RETURN lexbuf.lex_curr_p in last_token := Some token; token }
  | "break" { let token = BREAK lexbuf.lex_curr_p in last_token := Some token; token }
  | "continue" { let token = CONTINUE lexbuf.lex_curr_p in last_token := Some token; token }

  (* Operators *)
  | '+' { let token = PLUS lexbuf.lex_curr_p in last_token := Some token; token }
  | '-' { let token = MINUS lexbuf.lex_curr_p in last_token := Some token; token }
  | '=' { let token = EQUALS lexbuf.lex_curr_p in last_token := Some token; token }
  | '*' { let token = PRODUCT lexbuf.lex_curr_p in last_token := Some token; token }
  | '/' { let token = DIVIDE lexbuf.lex_curr_p in last_token := Some token; token }
  | '!' { let token = BANG lexbuf.lex_curr_p in last_token := Some token; token }
  | "not" { let token = NOT lexbuf.lex_curr_p in last_token := Some token; token }
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
        | SEMICOLON _ -> (private_read [@tailcall]) last_token state lexbuf
        | _ -> parse_semicolon ()
    )
  }
  | ':' { let token = COLON lexbuf.lex_curr_p in last_token := Some token; token }
  | '#' { read_comment lexbuf.lex_curr_p lexbuf }
  | '|' { let token = PIPE lexbuf.lex_curr_p in last_token := Some token; token }
  | '.' { let token = DOT lexbuf.lex_curr_p in last_token := Some token; token }

  (* String literals *)
  | '\'' {
    let token = read_string '\'' (Buffer.create string_buffer_size) lexbuf.lex_curr_p state lexbuf in
    last_token := Some token;
    token
  }
  | '"' {
    state := NotInterpolating :: !state;
    let token = read_string '"' (Buffer.create string_buffer_size) lexbuf.lex_curr_p state lexbuf in
    last_token := Some token;
    token
  }
  | '{' { let token = LCURLY lexbuf.lex_curr_p in last_token := Some token; token}
  | '}' {
    let token = (match List.hd !state with
    | NotInterpolating -> RCURLY lexbuf.lex_curr_p
    | Interpolating -> (read_string '"' (Buffer.create string_buffer_size) lexbuf.lex_curr_p state lexbuf)) in
    last_token := Some token;
    token
  }
  | '(' { let token = LPAREN lexbuf.lex_curr_p in last_token := Some token; token}
  | ')' { let token = RPAREN lexbuf.lex_curr_p in last_token := Some token; token}
  | ',' { let token = COMMA lexbuf.lex_curr_p in last_token := Some token; token}
  | '<' { let token = LESS lexbuf.lex_curr_p in last_token := Some token; token}
  | '>' { let token = GREATER lexbuf.lex_curr_p in last_token := Some token; token}
  | "<=" { let token = LEQ lexbuf.lex_curr_p in last_token := Some token; token}
  | ">=" { let token = GEQ lexbuf.lex_curr_p in last_token := Some token; token}
  | "==" { let token = DOUBLE_EQUALS lexbuf.lex_curr_p in last_token := Some token; token }
  | "!=" { let token = NOT_EQUALS lexbuf.lex_curr_p in last_token := Some token; token }
  | "<-" { let token = LEFT_ARROW lexbuf.lex_curr_p in last_token := Some token; token }
  | "->" { let token = RIGHT_ARROW lexbuf.lex_curr_p in last_token := Some token; token }
  | '[' { let token = LBRACKET lexbuf.lex_curr_p in last_token := Some token; token}
  | ']' { let token = RBRACKET lexbuf.lex_curr_p in last_token := Some token; token}
  (* Lexing.lexeme means return the string that matched the pattern *)
  | id { let token = ID (Lexing.lexeme lexbuf, lexbuf.lex_curr_p) in last_token := Some token; token }
  | context_id { let token = CONTEXT_ID (Lexing.lexeme lexbuf, lexbuf.lex_curr_p) in last_token := Some token; token }
  | prototype { let token = PROTOTYPE (Lexing.lexeme lexbuf, lexbuf.lex_curr_p) in last_token := Some token; token }
  | num { let token = NUM (float_of_string (Lexing.lexeme lexbuf), lexbuf.lex_curr_p) in last_token := Some token; token }
  | _ { syntax_error (SyntaxError (Lexing.lexeme lexbuf, lexbuf.lex_curr_p))}
  (* Here `eof` is a special regex built into ocamllex *)
  | eof {
    (* Note: if needed, a semicolon will be inserted later in filtering
       between lexing & parsing *)
    EOF lexbuf.lex_curr_p
  }

and read_comment pos =
  parse
  | '\n' {
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with
      pos_lnum = lexbuf.lex_curr_p.pos_lnum + 1;
      pos_bol = lexbuf.lex_curr_p.pos_cnum;
    };

    COMMENT (pos, None)
  }
  | eof {
    COMMENT (pos, Some lexbuf.lex_curr_p)
  }
  | [^ '\n']+ {
    (read_comment[@tailcall]) pos lexbuf
  }

and read_string delimiter buf pos state =
  (* TODO implement escapes *)
  parse
  | '"' as cur_char {
    if cur_char = delimiter then
      let hd = List.hd !state in
      (* Pop off top of the stack *)
      state := List.tl !state;
      (match hd with
      | NotInterpolating -> STRING_FULL (Buffer.contents buf, pos)
      | Interpolating -> (
        STRING_END (Buffer.contents buf, pos)
      ))
    else
      (Buffer.add_char buf cur_char;
      (read_string[@tailcall]) delimiter buf pos state lexbuf)
  }
  | "${" {
    if delimiter = '\'' then (
      Buffer.add_string buf (Lexing.lexeme lexbuf);
      (read_string[@tailcall]) delimiter buf pos state lexbuf)
    else
      let hd = List.hd !state in
        (match hd with
        | NotInterpolating -> (
          (* Change HEAD value *)
          state := (Interpolating :: List.tl !state);
          STRING_START (Buffer.contents buf, pos)
        )
        | Interpolating -> STRING_MIDDLE (Buffer.contents buf, pos))
  }
  | '\'' as cur_char {
    if cur_char = delimiter then
      STRING_FULL (Buffer.contents buf, pos)
    else
      (Buffer.add_char buf cur_char;
      (read_string[@tailcall]) delimiter buf pos state lexbuf)
  }
  | '\\' 'n' {
    if delimiter = '\'' then (
      Buffer.add_string buf (Lexing.lexeme lexbuf);
      (read_string[@tailcall]) delimiter buf pos state lexbuf)
    else (
      Buffer.add_char buf '\n';
      (read_string[@tailcall]) delimiter buf pos state lexbuf
    )
  }
  | '\\' 't' {
    if delimiter = '\'' then (
      Buffer.add_string buf (Lexing.lexeme lexbuf);
      (read_string[@tailcall]) delimiter buf pos state lexbuf)
    else (
      Buffer.add_char buf '\t';
      (read_string[@tailcall]) delimiter buf pos state lexbuf
    )
  }
  | '\\' 'r' {
    if delimiter = '\'' then (
      Buffer.add_string buf (Lexing.lexeme lexbuf);
      (read_string[@tailcall]) delimiter buf pos state lexbuf)
    else (
      Buffer.add_char buf '\r';
      (read_string[@tailcall]) delimiter buf pos state lexbuf
    )
  }
  | '\\' '\\' {
    if delimiter = '\'' then (
      Buffer.add_string buf (Lexing.lexeme lexbuf);
      (read_string[@tailcall]) delimiter buf pos state lexbuf)
    else (
      Buffer.add_char buf '\\';
      (read_string[@tailcall]) delimiter buf pos state lexbuf
    )
  }
  | '\\' '"' {
    if delimiter = '\'' then (
      Buffer.add_string buf (Lexing.lexeme lexbuf);
      (read_string[@tailcall]) delimiter buf pos state lexbuf)
    else (
      Buffer.add_char buf '"';
      (read_string[@tailcall]) delimiter buf pos state lexbuf
    )
  }
  | '$' {
    Buffer.add_char buf '$';
    (read_string[@tailcall]) delimiter buf pos state lexbuf
  }
  | [^ '"' '\\' '\'' '$']+ {
    let chunk = (Lexing.lexeme lexbuf) in
    Buffer.add_string buf chunk;
    (read_string[@tailcall]) delimiter buf pos state lexbuf
  }

(* Footer *)
{
  type lex_filter_state = False | True of Lexing.position

  (* insert semicolon before EOF *)
  let make_lex_filter () =
    let r = ref None in
    let last_token = ref None in
    let state = ref [ NotInterpolating ] in
    let should_spit_eof = ref False in
    (* insert semicolon before EOF *)
    let rec filter = fun buf ->
      let open Parser in
      match !should_spit_eof with
      | True pos -> EOF pos
      | False -> (
          let token = private_read r state buf in
          match token with
          | EOF pos -> (
              match !last_token with
              | None -> EOF pos
              | Some last_token -> (
                  match last_token with
                  | SEMICOLON _ -> EOF pos
                  | _ ->
                      should_spit_eof := True pos;
                      SEMICOLON pos))
          (* TODO do we need to unpack the optional EOF pos? *)
          | COMMENT _ -> (filter[@tailcall]) buf
          | _ ->
              last_token := Some token;
              token)
    in
    filter

  let bootstrap input =
    let filter = make_lex_filter () in
    let lexbuf = Lexing.from_string input in
    (filter, lexbuf)
}
