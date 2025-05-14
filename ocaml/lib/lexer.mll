(* header *)
{
open Parser
}

(* identifiers *)
let white = [' ' '\t' '\n']+
let digit = ['0'-'9']
let num = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+

(* rule and parse are keywords *)
rule read =
  parse
  (* means if `white` matches, call the read rule again and return its
     results--i.e. skip this match *)
  | white { read lexbuf }
  | "true" { TRUE }
  | "false" { FALSE }
  | "<=" { LEQ }
  | "*" { TIMES }
  | "+" { PLUS }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "let" { LET }
  | "=" { EQUALS }
  | "in" { IN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  (* Lexing.lexeme means return the string that matched the pattern *)
  | id { ID (Lexing.lexeme lexbuf) }
  | num { NUM (float_of_string (Lexing.lexeme lexbuf)) }
  (* Here `eof` is a special regex built into ocamllex *)
  | eof { EOF }
