(* Menhir parser *)

(* header *)
%{
  open Ast
%}

(* Declarations *)
(* In OCaml, `float` is a 64-bit IEEE float *)
%token <float> NUM
%token <string> ID
%token <string> STRING
%token TRUE
%token FALSE
%token PLUS
%token LET
%token EQUALS
%token EOF
%token SEMICOLON
%token FUNC
%token LCURLY
%token RCURLY
%token LPAREN
%token RPAREN
%token COMMA
(*
%token LEQ
%token TIMES
%token IN
%token IF
%token THEN
%token ELSE
  *)

(* Disambiguate precedence and associativity *)
(* TODO figure this out *)
%left PLUS
(*
%nonassoc IN
%nonassoc ELSE
%left LEQ
%left TIMES
*)

(* Declare the starting point for parsing (root of AST) *)
%start <Ast.stmt list> prog

%%

prog:
  | EOF { [] }
  | p = stmts; EOF { p }
  ;

stmts:
  | tl = stmts; hd = stmt { hd :: tl }
  | s = stmt { [s] }
  ;

stmt:
  | e1 = expr; SEMICOLON { ExprStmt e1 }
  | LET; id = ID; EQUALS; e1 = expr; SEMICOLON { LetStmt (id, e1) }
  (* Does not require semi-colon *)
  | FUNC; i = ID; p = parameter_list ; b = block { FuncStmt {name = i; parameters = p; block = b;} }
  ;

expr:
  | e1 = expr; PLUS; e2 = expr { Binary (Add, e1, e2) }
  | f = NUM { Num f }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | s = STRING { String s }
  | i = ID; LPAREN; a = argument_list; RPAREN { FuncInvoc (i, a) }
  | i = ID; LPAREN; RPAREN { FuncInvoc (i, []) }
  | i = ID { IdRef i }
  (*
  | e1 = expr; LEQ; e2 = expr { Binary (Leq, e1, e2) }
  | e1 = expr; TIMES; e2 = expr { Binary (Mult, e1, e2) }
  | LET; x = ID; EQUALS; e1 = expr; IN; e2 = expr { Let (x, e1, e2) }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If (e1, e2, e3) }
  | LPAREN; e=expr; RPAREN {e}
  *)
  ;

block:
  | LCURLY; RCURLY { [] }
  | LCURLY; s = stmts; RCURLY { s }

argument_list:
  (* TODO: support actual args *)
  | e = expr { [e] }
  | tl = argument_list; COMMA; e = expr { e :: tl }

parameter_list:
  (* TODO: support actual params *)
  (* TODO: make parameters different from just strings, to support type
     annotations *)
  | LPAREN; RPAREN { [] }
