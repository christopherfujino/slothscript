(* header *)
{
open Parser

exception SyntaxError of string * Lexing.position

type globalLexerState =
  | NotInterpolating
  | Interpolating

let string_buffer_size = 33

(* TODO figure out how to derive this *)
let to_s = function
| BANG _ -> "BANG"
| AMPERSAND _ -> "AMPERSAND"
| LEFT_ARROW _ -> "LEFT_ARROW"
| RIGHT_ARROW _ -> "RIGHT_ARROW"
| NOT _ -> "NOT"
| AND _ -> "AND"
| OR _ -> "OR"
| DOT _ -> "DOT"
| PIPE _ -> "PIPE"
| COLON _ -> "COLON"
| SEMICOLON _ -> "SEMICOLON"
| TRUE _ -> "TRUE"
| RPAREN _ -> "RPAREN"
| RCURLY _ -> "RCURLY"
| RBRACKET _ -> "RBRACKET"
| PRODUCT _ -> "PRODUCT"
| MODULO _ -> "MODULO"
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
    let semicolon = SEMICOLON lexbuf.lex_curr_p in
    match !last_token with
    | None -> (private_read [@tailcall]) last_token state lexbuf
    | Some t -> (match t with
        | ID _ -> semicolon
        | CONTEXT_ID _ -> semicolon
        | RPAREN _ -> semicolon
        | RCURLY _ -> semicolon
        | RBRACKET _ -> semicolon
        | NUM _ -> semicolon
        | STRING_FULL _ -> semicolon
        | STRING_END _ -> semicolon
        | NULL _ -> semicolon
        | RETURN _ -> semicolon
        | BREAK _ -> semicolon
        | CONTINUE _ -> semicolon
        | TRUE _ -> semicolon
        | FALSE _ -> semicolon
        (* ! is a postfix operator *)
        | BANG _ -> semicolon
        | AMPERSAND _ -> semicolon
        | _ ->
            (private_read [@tailcall]) last_token state lexbuf
    )
  }
  (* Literals *)
  | "true" { TRUE lexbuf.lex_start_p }
  | "false" { FALSE lexbuf.lex_start_p }
  | "null" { NULL lexbuf.lex_start_p }

  (* Keywords *)
  | "let" { LET lexbuf.lex_start_p }
  | "func" { FUNC lexbuf.lex_start_p }
  | "if" { IF lexbuf.lex_start_p }
  | "else" { ELSE lexbuf.lex_start_p }
  | "in" { IN lexbuf.lex_start_p }
  | "do" { DO lexbuf.lex_start_p }
  | "for" { FOR lexbuf.lex_start_p }
  | "with" { WITH lexbuf.lex_start_p }
  | "return" { RETURN lexbuf.lex_start_p }
  | "break" { BREAK lexbuf.lex_start_p }
  | "continue" { CONTINUE lexbuf.lex_start_p }

  (* Operators *)
  | '+' { PLUS lexbuf.lex_start_p }
  | '-' { MINUS lexbuf.lex_start_p }
  | '=' { EQUALS lexbuf.lex_start_p }
  | '*' { PRODUCT lexbuf.lex_start_p }
  | '/' { DIVIDE lexbuf.lex_start_p }
  | '%' { MODULO lexbuf.lex_start_p }
  | '!' { BANG lexbuf.lex_start_p }
  | '&' { AMPERSAND lexbuf.lex_start_p }
  | "not" { NOT lexbuf.lex_start_p }
  | "and" { AND lexbuf.lex_start_p }
  | "or" { OR lexbuf.lex_start_p }
  | ';' {
    let semicolon = SEMICOLON lexbuf.lex_start_p in
    match !last_token with
    | None ->
        (* TODO should we ignore leading semicolon? *) semicolon
    | Some t -> (match t with
        (* Allow no-op repeat semicolons *)
        | SEMICOLON _ ->
            (private_read [@tailcall]) last_token state lexbuf
        | _ ->
            semicolon
    )
  }
  | ':' { COLON lexbuf.lex_start_p }
  | '#' { read_comment lexbuf.lex_start_p lexbuf }
  | '|' { PIPE lexbuf.lex_start_p }
  | '.' { DOT lexbuf.lex_start_p }

  (* String literals *)
  | '\'' {
    read_string '\'' (Buffer.create string_buffer_size) lexbuf.lex_start_p state lexbuf
  }
  | '"' {
    state := NotInterpolating :: !state;
    read_string '"' (Buffer.create string_buffer_size) lexbuf.lex_start_p state lexbuf
  }
  | '{' { LCURLY lexbuf.lex_start_p}
  | '}' {
    match List.hd !state with
    | NotInterpolating -> RCURLY lexbuf.lex_start_p
    | Interpolating -> (read_string
      '"'
      (Buffer.create string_buffer_size)
      lexbuf.lex_start_p
      state
      lexbuf)
  }
  | '(' { LPAREN lexbuf.lex_start_p}
  | ')' { RPAREN lexbuf.lex_start_p}
  | ',' { COMMA lexbuf.lex_start_p}
  | '<' { LESS lexbuf.lex_start_p}
  | '>' { GREATER lexbuf.lex_start_p}
  | "<=" { LEQ lexbuf.lex_start_p}
  | ">=" { GEQ lexbuf.lex_start_p}
  | "==" { DOUBLE_EQUALS lexbuf.lex_start_p }
  | "!=" { NOT_EQUALS lexbuf.lex_start_p }
  | "<-" { LEFT_ARROW lexbuf.lex_start_p }
  | "->" { RIGHT_ARROW lexbuf.lex_start_p }
  | '[' { LBRACKET lexbuf.lex_start_p}
  | ']' { RBRACKET lexbuf.lex_start_p}
  (* Lexing.lexeme means return the string that matched the pattern *)
  | id as lexeme { ID (lexeme, lexbuf.lex_start_p) }
  | context_id as lexeme { CONTEXT_ID (lexeme, lexbuf.lex_start_p) }
  | prototype as lexeme { PROTOTYPE (lexeme, lexbuf.lex_start_p) }
  | num as lexeme { NUM (float_of_string lexeme, lexbuf.lex_start_p) }
  | _ { raise (SyntaxError (Lexing.lexeme lexbuf, lexbuf.lex_start_p))}
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
  | '\\' _ {
    let chunk = (Lexing.lexeme lexbuf) in
    Buffer.add_string buf chunk;
    (read_string[@tailcall]) delimiter buf pos state lexbuf
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
  | eof { EOF lexbuf.lex_curr_p }

(* Footer *)
{
  type lex_filter_state = False | True of Lexing.position

  let token_to_s = to_s

  (* insert semicolon before EOF *)
  let make_lex_filter () =
    let last_token = ref None in
    let state = ref [ NotInterpolating ] in
    let should_spit_eof = ref False in
    (* insert semicolon before EOF *)
    let rec filter = fun buf ->
      let open Parser in
      match !should_spit_eof with
      | True pos -> EOF pos
      | False -> (
          let token = try
            private_read last_token state buf
          with
          | Failure msg -> raise @@ Sloth_common.Common.LexerError msg
          in
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
