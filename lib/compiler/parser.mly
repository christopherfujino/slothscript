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
%token IF
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
%start <Ast.decl list> prog

%%

prog:
  | EOF { [] }
  | p = declarations; EOF { p }
  ;

declarations:
  | tl = declarations; hd = decl { hd :: tl }
  | d = decl { [d] }
  ;

decl:
  | FUNC; i = ID; LPAREN; RPAREN; b = block { FuncDecl {name = i; parameters = []; block = b;} }
  | FUNC; i = ID; LPAREN; p = parameter_list ; RPAREN; b = block { FuncDecl {name = i; parameters = p; block = b;} }
  | s = stmt { StmtDecl s }
  ;

stmts:
  | tl = stmts; hd = stmt {hd :: tl}
  | s = stmt { [s] }
  ;

stmt:
  | e1 = expr; SEMICOLON { ExprStmt e1 }
  | LET; id = ID; EQUALS; e1 = expr; SEMICOLON { LetStmt (id, e1) }
  | id = ID; EQUALS; e1 = expr; SEMICOLON { AssignStmt (id, e1) }
  ;

expr:
  | e1 = expr; PLUS; e2 = expr { Binary (Add, e1, e2) }
  | f = NUM { Num f }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | s = STRING { String s }
  | e = expr; LPAREN; a = argument_list; RPAREN { FuncInvoc (e, a) }
  | e = expr; LPAREN; RPAREN { FuncInvoc (e, []) }
  | i = ID { IdRef i }
  | FUNC; LPAREN; RPAREN; b = block { FuncExpr {parameters = []; block = b;} }
  | FUNC; LPAREN; p = parameter_list; RPAREN; b = block { FuncExpr {parameters = p; block = b;} }
  | IF; e1 = expr; b = block { IfExpr { conditional = e1; block = b } }
  (*
  | e1 = expr; LEQ; e2 = expr { Binary (Leq, e1, e2) }
  | e1 = expr; TIMES; e2 = expr { Binary (Mult, e1, e2) }
  | LET; x = ID; EQUALS; e1 = expr; IN; e2 = expr { Let (x, e1, e2) }
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
  (* TODO: make parameters different from just strings, to support type
     annotations *)
  | i = ID { [i] }
  | tl = parameter_list; COMMA; hd = ID { hd :: tl }
