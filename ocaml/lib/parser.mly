(* Menhir parser *)

(* header *)
%{
  open Ast
%}

(* Declarations *)
(* In OCaml, `float` is a 64-bit IEEE float *)
%token <float> NUM
%token <string> ID
%token TRUE
%token FALSE
%token LEQ
%token TIMES
%token PLUS
%token LPAREN
%token RPAREN
%token LET
%token EQUALS
%token IN
%token IF
%token THEN
%token ELSE
%token EOF
%token SEMICOLON

(* Disambiguate precedence and associativity *)
(* TODO figure this out *)
%nonassoc IN
%nonassoc ELSE
%left LEQ
%left PLUS
%left TIMES

(* Declare the starting point for parsing (root of AST) *)
%start <Ast.stmt> prog

%%

prog:
  | e1 = expr; SEMICOLON { ExprStmt e1 }
  (* match an expression and bind it to e, then return e
  | LET; x = ID; EQUALS; e1 = expr { LetStmt (x, e1) }
  ;
  *)

expr:
  | e1 = expr; PLUS; e2 = expr { Binary (Add, e1, e2) }
  (* upon an INT token, bind the contained Ocaml int val to i, and return AST
     node `Int i` *)
  | f = NUM { Num f }
  | TRUE { Bool true }
  | FALSE { Bool false }
  (*
  | x = ID { Var x }
  (* match three tokens in a row *)
  | e1 = expr; LEQ; e2 = expr { Binary (Leq, e1, e2) }
  | e1 = expr; TIMES; e2 = expr { Binary (Mult, e1, e2) }
  | LET; x = ID; EQUALS; e1 = expr; IN; e2 = expr { Let (x, e1, e2) }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If (e1, e2, e3) }
  | LPAREN; e=expr; RPAREN {e}
  *)
  ;
