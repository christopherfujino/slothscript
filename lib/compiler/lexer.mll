(* header *)
{
open Parser

exception SyntaxError of string
}

(* identifiers *)
let white = [' ' '\t' '\n']+
let digit = ['0'-'9']
let num = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

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
  | '+' { PLUS }
  | '=' { EQUALS }
  | ';' { SEMICOLON }
  | '"' { read_string (Buffer.create 33) lexbuf }
  | '{' { LCURLY }
  | '}' { RCURLY }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | ',' { COMMA }
  | "<=" { LEQ }
  | '-' { MINUS }
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
  | '"' { STRING (Buffer.contents buf) }
  | [^ '"']+ {
    let chunk = (Lexing.lexeme lexbuf) in
    Buffer.add_string buf chunk;
    read_string buf lexbuf }
