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
let white = [' ' '\t' '\n']+
let num = ('0'|(['1'-'9']['0'-'9']*)) ('.' ['0'-'9']+)?
let letter = ['a'-'z' 'A'-'Z']
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let interpolation_continuation = '"' '}'

(* rule and parse are keywords *)
rule read =
  parse
  (* means if `white` matches, call the read rule again and return its
     results--i.e. skip this match *)
  | white { (read [@tailcall]) lexbuf }
  | "true" { TRUE }
  | "false" { FALSE }
  | "null" { NULL }
  | "let" { LET }
  | "func" { FUNC }
  | "if" { IF }
  | "else" { ELSE }
  | "for" { FOR }
  | '+' { PLUS }
  | '=' { EQUALS }
  | '*' { PRODUCT }
  | '/' { DIVIDE }
  | ';' { SEMICOLON }
  | ':' { COLON }
  | interpolation_continuation {
    match !state with
    | NotInterpolating -> (
    failwith "TODO: figure out how to move back one on the lexbuf"
    (*read_string (Buffer.create string_buffer_size) lexbuf*)
    )
    | Interpolating -> read_string (Buffer.create string_buffer_size) lexbuf
  }
  | '"' {
    read_string (Buffer.create string_buffer_size) lexbuf
  }
  | '{' { LCURLY }
  | '}' {
    match !state with
    | NotInterpolating -> RCURLY
    | Interpolating -> read_string (Buffer.create string_buffer_size) lexbuf
  }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | ',' { COMMA }
  | '<' { LESS }
  | "<=" { LEQ }
  | '-' { MINUS }
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  (*
  | '.' { DOT }
  | '*' { TIMES }
  *)
  (* Lexing.lexeme means return the string that matched the pattern *)
  | id { ID (Lexing.lexeme lexbuf) }
  | num { NUM (float_of_string (Lexing.lexeme lexbuf)) }
  | _ { raise (SyntaxError (Lexing.lexeme lexbuf))}
  (* Here `eof` is a special regex built into ocamllex *)
  | eof { EOF }

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
