(* Menhir parser *)

(* header *)
%{
  open Ast
%}

(* Declarations *)
(* In OCaml, `float` is a 64-bit IEEE float *)
%token <float * Lexing.position> NUM
%token <string * Lexing.position> ID
%token <string * Lexing.position> CONTEXT_ID
%token <string * Lexing.position> PROTOTYPE
%token <string * Lexing.position> STRING_FULL
%token <string * Lexing.position> STRING_START
%token <string * Lexing.position> STRING_MIDDLE
%token <string * Lexing.position> STRING_END

(* Literals *)
%token <Lexing.position> TRUE
%token <Lexing.position> FALSE
%token <Lexing.position> NULL

(* Keywords *)
%token <Lexing.position> LET
%token <Lexing.position> FUNC
%token <Lexing.position> IF
%token <Lexing.position> ELSE
%token <Lexing.position> IN
%token <Lexing.position> DO
%token <Lexing.position> FOR
%token <Lexing.position> RETURN
%token <Lexing.position> BREAK
%token <Lexing.position> CONTINUE
%token <Lexing.position> WITH

(* Operators *)
%token <Lexing.position> PLUS
%token <Lexing.position> MINUS
%token <Lexing.position> EQUALS
%token <Lexing.position> DOUBLE_EQUALS
%token <Lexing.position> NOT_EQUALS
%token <Lexing.position> PRODUCT
%token <Lexing.position> DIVIDE
%token <Lexing.position> MODULO
%token <Lexing.position> PIPE
%token <Lexing.position> LEQ
%token <Lexing.position> GEQ
%token <Lexing.position> LESS
%token <Lexing.position> GREATER
%token <Lexing.position> BANG
%token <Lexing.position> NOT
%token <Lexing.position> AND
%token <Lexing.position> OR
%token <Lexing.position> LEFT_ARROW
%token <Lexing.position> RIGHT_ARROW

%token <Lexing.position> SEMICOLON
%token <Lexing.position> DOT
%token <Lexing.position> COLON
%token <Lexing.position> LCURLY
%token <Lexing.position> RCURLY
%token <Lexing.position> LPAREN
%token <Lexing.position> RPAREN
%token <Lexing.position> LBRACKET
%token <Lexing.position> RBRACKET
%token <Lexing.position> COMMA

(* These will be stripped out by the Lexer; optional position is of EOF *)
%token <Lexing.position * Lexing.position option> COMMENT

%token <Lexing.position> EOF

(* Disambiguate precedence and associativity *)
(* These are optional, and could have been done exclusively with production
   rules--however, they help resolve conflicts.

   These are ordered, from high to low precedence.
   *)

%left OR BANG (* Postfix *)
%left AND
%left LEFT_ARROW (* Prefix *) RIGHT_ARROW (* Postfix *)
%left NOT_EQUALS DOUBLE_EQUALS GEQ LEQ LESS GREATER
%left MINUS PLUS PIPE
%left PRODUCT DIVIDE MODULO
%left NOT (* Does associativity matter?! *)

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
  | FUNC; i = ID; LPAREN; RPAREN; b = block; SEMICOLON {
    let i, pos = i in
    FuncDecl {name = i; parameters = []; block = b; pos}
  }
  | FUNC; i = ID; LPAREN; p = parameter_list ; RPAREN; b = block; SEMICOLON {
    let i, pos = i in
    FuncDecl {name = i; parameters = p; block = b; pos} }
  | s = stmt { StmtDecl s }
  ;

stmts:
  | tl = stmts; hd = stmt {hd :: tl}
  | s = stmt { [s] }
  ;

stmt:
  | s = stmt_sans_semicolon; SEMICOLON { s }
  ;

(* Used in single statement blocks *)
stmt_sans_semicolon:
  | pos = RETURN; e = expr1 {
    BreakingStmt (Return, Some e, pos)
  }
  | pos = RETURN {
    BreakingStmt (Return, None, pos)
  }
  | pos = BREAK; e = expr1 {
    BreakingStmt (Break, Some e, pos)
  }
  | pos = BREAK {
    BreakingStmt (Break, None, pos)
  }
  | pos = CONTINUE; e = expr1 {
    BreakingStmt (Continue, Some e, pos)
  }
  | pos = CONTINUE {
    BreakingStmt (Continue, None, pos)
  }
  | e1 = expr1 { ExprStmt e1 }
  ;

(* Conditionals *)
expr1:
  | pos = FOR; init = expr1; SEMICOLON; comp = expr1; SEMICOLON; inc = expr1; bl = block {
    ForLoop (init, comp, inc, bl, pos)
  }
  | pos = FOR; i = ID; IN; iteratee = expr1; block = block {
    let (iterator_name, _) = i in
    ForInLoop {iterator_name; iteratee; block; pos}
  }
  | LET; id = ID; EQUALS; e1 = expr1 {
    let (id, pos) = id in
    LetExpr (id, e1, pos)
  }
  | id = ID; EQUALS; e1 = expr1 {
    let (id, pos) = id in
    AssignExpr (id, e1, pos) }
  | subscript = subscript; pos = EQUALS; rhs = expr1 {
    SubAssignExpr {subscript; value=rhs; pos }
  }

  (* assignment_list never has trailing comma *)
  | pos = WITH; LPAREN; assignments = assignment_list; COMMA; RPAREN; b = block {
    WithExpr (assignments, b, pos)
  }
  | pos = WITH; LPAREN; assignments = assignment_list; RPAREN; b = block {
    WithExpr (assignments, b, pos)
  }
  | e = expr2 { e }

(* Operators *)
expr2:
  (* Highest *)
  | e1 = expr2; pos = DOUBLE_EQUALS; e2 = expr2 {
    Equality (e1, e2, true, pos)
  }
  | e1 = expr2; pos = NOT_EQUALS; e2 = expr2 {
    Equality (e1, e2, false, pos)
  }
  | e1 = expr2; pos = LESS; e2 = expr2 {
    Binary ( e1, e2, Less, pos)
  }
  | e1 = expr2; pos = GREATER; e2 = expr2 {
    Binary ( e1, e2, Greater, pos)
  }
  | e1 = expr2; pos = LEQ; e2 = expr2 {
    Binary ( e1, e2, Leq, pos)
  }
  | e1 = expr2; pos = GEQ; e2 = expr2 {
    Binary ( e1, e2, Geq, pos)
  }

  | e1 = expr2; pos = PLUS; e2 = expr2 {
    Binary ( e1, e2, Plus, pos)
  }
  | e1 = expr2; pos = MINUS; e2 = expr2 {
    Binary ( e1, e2, Minus, pos)
  }
  | e1 = expr2; pos = PIPE; e2 = expr2 {
    Binary ( e1, e2, Pipe, pos)
  }

  | e1 = expr2; pos = PRODUCT; e2 = expr2 {
    Binary ( e1, e2, Product, pos)
  }
  | e1 = expr2; pos = DIVIDE; e2 = expr2 {
    Binary ( e1, e2, Divide, pos)
  }
  | e1 = expr2; pos = MODULO; e2 = expr2 {
    Binary ( e1, e2, Modulo, pos)
  }
  | e1 = expr2; pos = AND; e2 = expr2 {
    Binary ( e1, e2, And, pos)
  }
  | e1 = expr2; pos = OR; e2 = expr2 {
    Binary ( e1, e2, Or, pos)
  }

  | e1 = expr2; pos = RIGHT_ARROW; e2 = expr2 {
    Binary ( e1, e2, RightArrow, pos)
  }
  | pos = LEFT_ARROW; e = expr2 {
    UnaryExpr { target=e; operator=LeftArrow; pos}
  }
  | pos = NOT; e = expr2 {
    UnaryExpr { target=e; operator=Not; pos}
  }
  | e = expr2; pos = BANG {
    UnaryExpr { target = e; operator=Bang; pos}
  }
  | pos = MINUS; e = expr2 {
    UnaryExpr { target = e; operator=Minus; pos}
  }
 
  | e = expr7 { e }


(* function invocation *)
expr7:
  | e = expr7; pos = LPAREN; a = expr_list; RPAREN {
    FuncInvoc (e, a, pos)
  }
  | e = expr7; pos = LPAREN; RPAREN {
    FuncInvoc (e, [], pos)
  }
  | s = subscript { s }
  | e = expr7; pos = DOT; meth = ID {
    let (name, _) = meth in
    ObjDeref (e, name, pos)
  }
  | e = expr8 { e }
  ;


(* Primary - literals or grouping *)
expr8:
  | pos = DO; block = block {
    DoBlock (block, pos)
  }
  | pos = FUNC; LPAREN; RPAREN; b = block {
    FuncExpr {parameters = []; block = b; pos}
  }
  | pos = FUNC; LPAREN; p = parameter_list; RPAREN; b = block {
    FuncExpr {parameters = p; block = b; pos}
  }
  | l = list_literal { l }
  | f = NUM { let f, pos = f in Num (f, pos) }
  | pos = TRUE {
    Bool (true, pos)
  }
  | pos = FALSE {
    Bool (false, pos)
  }
  | pos = NULL {
    Null pos
  }
  (* TODO implement the rest of the string parts *)
  | ss = STRING_START; e = expr1; se = STRING_END {
    let (se, se_pos) = se in
    let end_part = EndStringInterp (e, se, se_pos) in
    let (ss, ss_pos) = ss in
    String (StartStringInterp (ss, end_part, ss_pos), ss_pos)
  }
  | ss = STRING_START; cont = string_middle; e = expr1; se = STRING_END {
    let (string_end_s, string_end_pos) = se in
    let end_part = EndStringInterp (e, string_end_s, string_end_pos) in
    (* We are iterating from back to front... *)
    let cont2_opt = List.fold_left (fun acc cur -> (
      let (e, s, pos) = cur in
      match acc with
      | None -> Some (MiddleStringInterp (e, s, end_part, pos))
      | Some prev -> Some (MiddleStringInterp (e, s, prev, pos))
    )) None cont in
    let (ss, ss_pos) = ss in
    String (StartStringInterp (ss, Option.get cont2_opt, ss_pos), ss_pos)
  }
  | s = STRING_FULL {
    let (s, pos) = s in
    String (FullString (s, pos), pos)
  }
  | i = ID {
    let (i, pos) = i in
    IdRef (i, pos)
  }
  | i = CONTEXT_ID {
    let (i, pos) = i in
    ContextId (i, pos)
  }
  | i = PROTOTYPE {
    let (i, pos) = i in
    ProtoRef (i, pos)
  }
  | LPAREN; e = expr1; RPAREN { e }
  | h = hash_literals { h }
  | pos = IF; e1 = expr1; b = block; cont = conditional_continuation {
    IfExpr (
      IfCont ({
        conditional = e1;
        block = b;
        continuation = Some cont;
        pos;
      }), pos)
  }
  | pos = IF; e1 = expr1; b = block {
    IfExpr (IfCont { conditional = e1; block = b; continuation = None; pos}, pos)
  }
  ;

assignment_list:
  | prev = assignment_list; COMMA; name = CONTEXT_ID; EQUALS; e = expr1 {
    let name, _ = name in
    (name, e) :: prev
  }
  | name = CONTEXT_ID; EQUALS; e = expr1 {
    let name, _ = name in
    [(name, e)]
  }

(* Returns reversed list *)
string_middle:
  | e = expr1 ; s = STRING_MIDDLE {
    let (s, pos) = s in
    [(e, s, pos)]
  }
  | cont = string_middle; e = expr1; s = STRING_MIDDLE {
    let (s, pos) = s in
    (e, s, pos) :: cont
  }

list_literal:
  | pos = LBRACKET; RBRACKET {
    List ([], pos)
  }
  (* expr_list will never end in COMMA *)
  | pos = LBRACKET; l = expr_list ; COMMA; RBRACKET {
    List (l, pos)
  }
  | pos = LBRACKET; l = expr_list ; RBRACKET {
    List (l, pos)
  }
  ;

hash_literals:
  | pos = LCURLY; RCURLY {
    HashMap ([], pos)
  }
  (* Allow single line literal without trailing comma *)
  | pos = LCURLY; k = expr1; COLON; v = expr1; RCURLY {
    HashMap ([(k, v)], pos)
  }
  | pos = LCURLY; p = hash_literal_pair; RCURLY {
    HashMap (p, pos)
  }
  ;

hash_literal_pair:
  (* Are other types of expressions allowed for keys? *)
  (* Note: trailing commas required because of ASI *)
  | k = expr1; COLON; v = expr1; COMMA { [(k, v)] }
  | p = hash_literal_pair; k = expr1; COLON; v = expr1; COMMA { (k, v) :: p }
  ;

(* Could be expr or stmt for assignment *)
subscript:
  | e = expr7; pos = LBRACKET; sub = expr1 ; RBRACKET {
    Subscript (e, sub, pos)
  }
  ;

conditional_continuation:
  | e = elif; pos = ELSE; b = block {
      let else_cont = ElseCont (b, pos) in
      match e with
      | IfCont {conditional; block; continuation=_; pos} -> IfCont { conditional; block; continuation=(Some else_cont); pos }
      | _ -> failwith "Unreachable"
  }
  | e = elif { e }
  | pos = ELSE; b = block {
      ElseCont (b, pos)
  }
  ;

elif:
  | prev = elif; pos = ELSE; IF; conditional = expr2; block = block {
    let cur = IfCont {conditional; block; continuation=None; pos} in
    match prev with
    | IfCont {conditional; block; continuation=_; pos} -> IfCont {conditional; block; continuation=(Some cur); pos}
    | _ -> failwith "Unreachable"
  }
  | pos = ELSE; IF; conditional = expr2; block = block {
    IfCont {conditional; block; continuation=None; pos}
  }

block:
  | LCURLY; RCURLY { [] }
  (* Allow a single statement block without a trailing semicolon *)
  | LCURLY; s = stmts; RCURLY { s }
  | LCURLY; s = stmt_sans_semicolon; RCURLY { [s] }

(* arg-list or list literal *)
expr_list:
  | e = expr1 { [e] }
  | tl = expr_list; COMMA; e = expr1 { e :: tl }

parameter_list:
  (* TODO: make parameters different from just strings, to support type
     annotations *)
  | i = ID {
    let (i, pos) = i in
    let i : string = i in
    let tuple = (i, pos) in
    [tuple]
  }
  | tl = parameter_list; COMMA; hd = ID {
    let (i, pos) = hd in
    (i, pos) :: tl
  }

%%

(* Footer *)
