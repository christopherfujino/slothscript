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
%token NULL
%token PLUS
%token MINUS
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
%token ELSE
%token LEQ
(*
%token DOT
%token TIMES
%token IN
%token THEN
  *)

(* Disambiguate precedence and associativity *)
(* These are optional, and could have been done exclusively with production
   rules. *)
%left PLUS
%left MINUS
%left LEQ
(*
%nonassoc ELSE
%nonassoc IN
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
  | e1 = expr1; SEMICOLON { ExprStmt e1 }
  | LET; id = ID; EQUALS; e1 = expr1; SEMICOLON { LetStmt (id, e1) }
  | id = ID; EQUALS; e1 = expr1; SEMICOLON { AssignStmt (id, e1) }
  ;

(* TODO make levels of expressions *)
expr1:
  | IF; e1 = expr2; b = block; cont = conditional_continuation { IfExpr (IfCont { conditional = e1; block = b; continuation = Some cont }) }
  | IF; e1 = expr2; b = block { IfExpr (IfCont { conditional = e1; block = b; continuation = None } ) }
  | e = expr2 { e }

expr2:
  | FUNC; LPAREN; RPAREN; b = block { FuncExpr {parameters = []; block = b;} }
  | FUNC; LPAREN; p = parameter_list; RPAREN; b = block { FuncExpr {parameters = p; block = b;} }
  | e1 = expr2; PLUS; e2 = expr2 {
    MethodInvoc { receiver=e1; target="+"; args=[e2] }
  }
  | e1 = expr2; LEQ; e2 = expr2 {
    MethodInvoc { receiver=e1; target="<="; args=[e2] }
  }
  | e1 = expr2; MINUS; e2 = expr2 {
    MethodInvoc { receiver=e1; target="-"; args=[e2] }
  }
  | e = expr3 { e }

expr3:
  | e = expr3; LPAREN; a = argument_list; RPAREN { FuncInvoc (e, a) }
  | e = expr3; LPAREN; RPAREN { FuncInvoc (e, []) }
  (*
  | e1 = expr; LEQ; e2 = expr { Binary (Leq, e1, e2) }
  | e1 = expr; TIMES; e2 = expr { Binary (Mult, e1, e2) }
  | LET; x = ID; EQUALS; e1 = expr; IN; e2 = expr { Let (x, e1, e2) }
  | LPAREN; e=expr; RPAREN {e}
  *)
  | e = expr4 { e }
  ;


(* Primary - literals or grouping *)
expr4:
  | f = NUM { Num f }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | NULL { Null }
  | s = STRING { String s }
  | i = ID { IdRef i }

conditional_continuation:
  | e = elif; ELSE; b = block {
      let else_cont = ElseCont b in
      match e with
      | IfCont {conditional; block; continuation=_} -> IfCont { conditional; block; continuation=(Some else_cont) }
      | _ -> failwith "Unreachable"
  }
  | e = elif { e }
  | ELSE; b = block { ElseCont b }
  ;

elif:
  | prev = elif; ELSE; IF; conditional = expr2; block = block {
    let cur = IfCont {conditional; block; continuation=None} in
    match prev with
    | IfCont {conditional; block; continuation=_} -> IfCont {conditional; block; continuation=(Some cur)}
    | _ -> failwith "Unreachable"
  }
  | ELSE; IF; conditional = expr2; block = block { IfCont {conditional; block; continuation=None } }

block:
  | LCURLY; RCURLY { [] }
  | LCURLY; s = stmts; RCURLY { s }

argument_list:
  (* TODO: support actual args *)
  | e = expr1 { [e] }
  | tl = argument_list; COMMA; e = expr1 { e :: tl }

parameter_list:
  (* TODO: make parameters different from just strings, to support type
     annotations *)
  | i = ID { [i] }
  | tl = parameter_list; COMMA; hd = ID { hd :: tl }
