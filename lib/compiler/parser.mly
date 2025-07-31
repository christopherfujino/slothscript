(* Menhir parser *)

(* header *)
%{
  open Ast
%}

(* Declarations *)
(* In OCaml, `float` is a 64-bit IEEE float *)
%token <float> NUM
%token <string> ID
%token <string> STRING_FULL
%token <string> STRING_START
%token <string> STRING_MIDDLE
%token <string> STRING_END
%token TRUE
%token FALSE
%token NULL
%token PLUS
%token MINUS
%token PRODUCT
%token DIVIDE
%token LET
%token FOR
%token EQUALS
%token EOF
%token COLON
%token SEMICOLON
%token FUNC
%token LCURLY
%token RCURLY
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token COMMA
%token IF
%token ELSE
%token LEQ
%token LESS
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
%left PRODUCT
%left DIVIDE
%left LEQ
%left LESS
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
  | FUNC; i = ID; LPAREN; RPAREN; b = block; SEMICOLON { FuncDecl {name = i; parameters = []; block = b;} }
  | FUNC; i = ID; LPAREN; p = parameter_list ; RPAREN; b = block; SEMICOLON { FuncDecl {name = i; parameters = p; block = b;} }
  | s = stmt { StmtDecl s }
  ;

stmts:
  | tl = stmts; hd = stmt {hd :: tl}
  | s = stmt { [s] }
  ;

stmt:
  | s = stmt_sans_semicolon; SEMICOLON { s }
  | FOR; st = stmt; comp = expr1; SEMICOLON; inc = stmt_sans_semicolon; bl = block; SEMICOLON {
    ForLoop (st, comp, inc, bl)
  }
  ;

(* Used in for loops *)
stmt_sans_semicolon:
  | e1 = expr1 { ExprStmt e1 }
  | LET; id = ID; EQUALS; e1 = expr1 { LetStmt (id, e1) }
  | id = ID; EQUALS; e1 = expr1 { AssignStmt (id, e1) }
  | subscript = subscript; EQUALS; rhs = expr1 { SubAssignStmt {subscript; value=rhs } }
  ;

(* Conditionals *)
expr1:
  | IF; e1 = expr2; b = block; cont = conditional_continuation {
    IfExpr (
      IfCont {
        conditional = e1;
        block = b;
        continuation = Some cont;
      }
    )
  }
  | IF; e1 = expr2; b = block { IfExpr (IfCont { conditional = e1; block = b; continuation = None } ) }
  | e = expr2 { e }

(* closure literals and infix funcs *)
expr2:
  | FUNC; LPAREN; RPAREN; b = block { FuncExpr {parameters = []; block = b;} }
  | FUNC; LPAREN; p = parameter_list; RPAREN; b = block { FuncExpr {parameters = p; block = b;} }
  | e1 = expr2; PLUS; e2 = expr2 {
    MethodInvoc { receiver=e1; target="+"; args=[e2] }
  }
  | e1 = expr2; LESS; e2 = expr2 {
    MethodInvoc { receiver=e1; target="<"; args=[e2] }
  }
  | e1 = expr2; LEQ; e2 = expr2 {
    MethodInvoc { receiver=e1; target="<="; args=[e2] }
  }
  | e1 = expr2; MINUS; e2 = expr2 {
    MethodInvoc { receiver=e1; target="-"; args=[e2] }
  }
  | e1 = expr2; PRODUCT; e2 = expr2 {
    MethodInvoc { receiver=e1; target="*"; args=[e2] }
  }
  | e1 = expr2; DIVIDE; e2 = expr2 {
    MethodInvoc { receiver=e1; target="/"; args=[e2] }
  }
  | e = expr3 { e }

(* function invocation *)
expr3:
  | e = expr3; LPAREN; a = expr_list; RPAREN { FuncInvoc (e, a) }
  | e = expr3; LPAREN; RPAREN { FuncInvoc (e, []) }
  | s = subscript { s }
  | e = expr4 { e }
  ;


(* Primary - literals or grouping *)
expr4:
  | l = list_literals { l }
  | f = NUM { Num f }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | NULL { Null }
  (* TODO implement the rest of the string parts *)
  | ss = STRING_START; e = expr1; se = STRING_END {
    let end_part = EndStringInterp (e, se) in
    String (StartStringInterp (ss, end_part))
  }
  | ss = STRING_START; cont = string_middle; e = expr1; se = STRING_END {
    let end_part = EndStringInterp (e, se) in
    (* We are iterating from back to front... *)
    let cont2_opt = List.fold_left (fun acc cur -> (
      let (e, s) = cur in
      match acc with
      | None -> Some (MiddleStringInterp (e, s, end_part))
      | Some prev -> Some (MiddleStringInterp (e, s, prev))
    )) None cont in
    String (StartStringInterp (ss, Option.get cont2_opt))
  }
  | s = STRING_FULL { String (FullString s) }
  | i = ID { IdRef i }
  | LPAREN; e = expr1; RPAREN { e }
  | h = hash_literals { h }
  ;

(* Returns reversed list *)
string_middle:
  | e = expr1 ; s = STRING_MIDDLE { [(e, s)] }
  | cont = string_middle; e = expr1; s = STRING_MIDDLE {
    (e, s) :: cont
  }

list_literals:
  | LBRACKET; RBRACKET { List [] }
  | LBRACKET; l = expr_list ; RBRACKET { List l }
  ;

hash_literals:
  | LCURLY; RCURLY { HashMap [] }
  ;

hash_literal_pair:
  (* Are other types of expressions allowed for keys? *)
  (* TODO allow non-trailing comma? *)
  | k = expr4; COLON; v = expr1; COMMA { [(k, v)] }
  ;

(* Could be expr or stmt for assignment *)
subscript:
  | e = expr3; LBRACKET; sub = expr1 ; RBRACKET { Subscript (e, sub) }
  ;

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

(* arg-list or list literal *)
expr_list:
  (* TODO: support actual args *)
  | e = expr1 { [e] }
  | tl = expr_list; COMMA; e = expr1 { e :: tl }

parameter_list:
  (* TODO: make parameters different from just strings, to support type
     annotations *)
  | i = ID { [i] }
  | tl = parameter_list; COMMA; hd = ID { hd :: tl }
