(* Menhir parser *)

(* header *)
%{
  open Ast
  open Sloth_common.Position
%}

(* Declarations *)
(* In OCaml, `float` is a 64-bit IEEE float *)
%token <float * Lexing.position> NUM
%token <string * Lexing.position> ID
%token <string * Lexing.position> PROTOTYPE
%token <string * Lexing.position> STRING_FULL
%token <string * Lexing.position> STRING_START
%token <string * Lexing.position> STRING_MIDDLE
%token <string * Lexing.position> STRING_END
%token <Lexing.position> TRUE
%token <Lexing.position> FALSE
%token <Lexing.position> NULL
%token <Lexing.position> LET
%token <Lexing.position> FUNC
%token <Lexing.position> IF
%token <Lexing.position> ELSE
%token <Lexing.position> IN
%token <Lexing.position> DO
%token <Lexing.position> FOR
%token <Lexing.position> PLUS
%token <Lexing.position> MINUS
%token <Lexing.position> EQUALS
%token <Lexing.position> DOUBLE_EQUALS
%token <Lexing.position> NOT_EQUALS
%token <Lexing.position> PRODUCT
%token <Lexing.position> DIVIDE
%token <Lexing.position> SEMICOLON

%token <Lexing.position> PIPE
%token <Lexing.position> DOT
%token <Lexing.position> COLON
%token <Lexing.position> LCURLY
%token <Lexing.position> RCURLY
%token <Lexing.position> LPAREN
%token <Lexing.position> RPAREN
%token <Lexing.position> LBRACKET
%token <Lexing.position> RBRACKET
%token <Lexing.position> COMMA
%token <Lexing.position> LEQ
%token <Lexing.position> GEQ
%token <Lexing.position> LESS
%token <Lexing.position> GREATER
%token <Lexing.position> BANG

(* These will be stripped out by the Lexer; optional position is of EOF *)
%token <Lexing.position * Lexing.position option> COMMENT

%token <Lexing.position> EOF

(* Disambiguate precedence and associativity *)
(* These are optional, and could have been done exclusively with production
   rules--however, they help resolve conflicts.

   These are ordered, from high to low precedence.
   *)

%left BANG
%left NOT_EQUALS DOUBLE_EQUALS GEQ LEQ LESS GREATER
%left MINUS PLUS PIPE
%left PRODUCT DIVIDE

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
    let pos = Sloth_common.Position.t_of_lexing_position pos in
    FuncDecl {name = i; parameters = []; block = b; pos}
  }
  | FUNC; i = ID; LPAREN; p = parameter_list ; RPAREN; b = block; SEMICOLON {
    let i, pos = i in
    let pos = Sloth_common.Position.t_of_lexing_position pos in
    FuncDecl {name = i; parameters = p; block = b; pos} }
  | s = stmt { StmtDecl s }
  ;

stmts:
  | tl = stmts; hd = stmt {hd :: tl}
  | s = stmt { [s] }
  ;

stmt:
  | s = stmt_sans_semicolon; SEMICOLON { s }
  | pos = FOR; st = stmt; comp = expr1; SEMICOLON; inc = stmt_sans_semicolon; bl = block; SEMICOLON {
    let pos = Sloth_common.Position.t_of_lexing_position pos in
    ForLoop (st, comp, inc, bl, pos)
  }
  | pos = FOR; i = ID; IN; iteratee = expr1; block = block SEMICOLON {
    let (iterator_name, _) = i in
    let pos = Sloth_common.Position.t_of_lexing_position pos in
    ForInLoop {iterator_name; iteratee; block; pos}
  }
  ;

(* Used in for loops *)
stmt_sans_semicolon:
  | e1 = expr1 { ExprStmt e1 }
  | LET; id = ID; EQUALS; e1 = expr1 {
    let (id, pos) = id in
    let pos = Sloth_common.Position.t_of_lexing_position pos in
    LetStmt (id, e1, pos)
  }
  | id = ID; EQUALS; e1 = expr1 {
    let (id, pos) = id in
    let pos = Sloth_common.Position.t_of_lexing_position pos in
    AssignStmt (id, e1, pos) }
  | subscript = subscript; pos = EQUALS; rhs = expr1 {
    let pos = Sloth_common.Position.t_of_lexing_position pos in
    SubAssignStmt {subscript; value=rhs; pos }
  }
  ;

(* Conditionals *)
expr1:
  | pos = IF; e1 = expr2; b = block; cont = conditional_continuation {
    let pos = Sloth_common.Position.t_of_lexing_position pos in
    IfExpr (
      IfCont ({
        conditional = e1;
        block = b;
        continuation = Some cont;
        pos;
      }), pos)
  }
  | pos = IF; e1 = expr2; b = block {
    let pos = Sloth_common.Position.t_of_lexing_position pos in
    IfExpr (IfCont { conditional = e1; block = b; continuation = None; pos}, pos)
  }
  | e = expr2 { e }

(* Operators *)
expr2:
  (* Highest *)
  | e1 = expr2; pos = DOUBLE_EQUALS; e2 = expr2 {
    let pos = Sloth_common.Position.t_of_lexing_position pos in
    Equality (e1, e2, true, pos)
  }
  | e1 = expr2; pos = NOT_EQUALS; e2 = expr2 {
    let pos = Sloth_common.Position.t_of_lexing_position pos in
    Equality (e1, e2, false, pos)
  }
  | e1 = expr2; pos = LESS; e2 = expr2 {
    let pos = Sloth_common.Position.t_of_lexing_position pos in
    MethodInvoc { receiver=e1; target="<"; args=[e2]; pos}
  }
  | e1 = expr2; pos = GREATER; e2 = expr2 {
    let pos = Sloth_common.Position.t_of_lexing_position pos in
    MethodInvoc { receiver=e1; target=">"; args=[e2]; pos}
  }
  | e1 = expr2; pos = LEQ; e2 = expr2 {
    let pos = Sloth_common.Position.t_of_lexing_position pos in
    MethodInvoc { receiver=e1; target="<="; args=[e2]; pos}
  }
  | e1 = expr2; pos = GEQ; e2 = expr2 {
    let pos = Sloth_common.Position.t_of_lexing_position pos in
    MethodInvoc { receiver=e1; target=">="; args=[e2]; pos}
  }

  | e1 = expr2; pos = PLUS; e2 = expr2 {
    let pos = Sloth_common.Position.t_of_lexing_position pos in
    MethodInvoc { receiver=e1; target="+"; args=[e2]; pos}
  }
  | e1 = expr2; pos = MINUS; e2 = expr2 {
    let pos = Sloth_common.Position.t_of_lexing_position pos in
    MethodInvoc { receiver=e1; target="-"; args=[e2]; pos}
  }
  | e1 = expr2; pos = PIPE; e2 = expr2 {
    let pos = Sloth_common.Position.t_of_lexing_position pos in
    MethodInvoc { receiver=e1; target="|"; args=[e2]; pos}
  }

  (* operators *, /; TODO add % *)
  | e1 = expr2; pos = PRODUCT; e2 = expr2 {
    let pos = Sloth_common.Position.t_of_lexing_position pos in
    MethodInvoc { receiver=e1; target="*"; args=[e2]; pos}
  }
  | e1 = expr2; pos = DIVIDE; e2 = expr2 {
    let pos = Sloth_common.Position.t_of_lexing_position pos in
    MethodInvoc { receiver=e1; target="/"; args=[e2]; pos}
  }

  (* ! *)
  | pos = BANG; e = expr2 {
    let pos = Sloth_common.Position.t_of_lexing_position pos in
    UnaryExpr { target=e; is_prefix=true; operator=Bang; pos}
  }

  | e = expr2; pos = BANG {
    let pos = Sloth_common.Position.t_of_lexing_position pos in
    UnaryExpr { target = e; is_prefix=false; operator=Bang; pos}
  }
 
  | e = expr7 { e }


(* function invocation *)
expr7:
  | e = expr7; pos = LPAREN; a = expr_list; RPAREN {
    let pos = Sloth_common.Position.t_of_lexing_position pos in
    FuncInvoc (e, a, pos)
  }
  | e = expr7; pos = LPAREN; RPAREN {
    let pos = Sloth_common.Position.t_of_lexing_position pos in
    FuncInvoc (e, [], pos)
  }
  | s = subscript { s }
  | e = expr8; pos = DOT; meth = ID {
    let (name, _) = meth in
    let pos = Sloth_common.Position.t_of_lexing_position pos in
    ObjDeref (e, name, pos)
  }
  | e = expr8 { e }
  ;


(* Primary - literals or grouping *)
expr8:
  | pos = DO; block = block {
    let pos = Sloth_common.Position.t_of_lexing_position pos in
    DoBlock (block, pos)
  }
  | pos = FUNC; LPAREN; RPAREN; b = block {
    let pos = Sloth_common.Position.t_of_lexing_position pos in
    FuncExpr {parameters = []; block = b; pos}
  }
  | pos = FUNC; LPAREN; p = parameter_list; RPAREN; b = block {
    let pos = Sloth_common.Position.t_of_lexing_position pos in
    FuncExpr {parameters = p; block = b; pos}
  }
  | l = list_literal { l }
  | f = NUM { let f, pos = f in Num (f, t_of_lexing_position pos) }
  | pos = TRUE {
    let pos = Sloth_common.Position.t_of_lexing_position pos in
    Bool (true, pos)
  }
  | pos = FALSE {
    let pos = Sloth_common.Position.t_of_lexing_position pos in
    Bool (false, pos)
  }
  | pos = NULL {
    let pos = t_of_lexing_position pos in
    Null pos
  }
  (* TODO implement the rest of the string parts *)
  | ss = STRING_START; e = expr1; se = STRING_END {
    let (se, se_pos) = se in
    let se_pos = t_of_lexing_position se_pos in
    let end_part = EndStringInterp (e, se, se_pos) in
    let (ss, ss_pos) = ss in
    let ss_pos = t_of_lexing_position ss_pos in
    String (StartStringInterp (ss, end_part, ss_pos), ss_pos)
  }
  | ss = STRING_START; cont = string_middle; e = expr1; se = STRING_END {
    let (string_end_s, string_end_pos) = se in
    let string_end_pos = t_of_lexing_position string_end_pos in
    let end_part = EndStringInterp (e, string_end_s, string_end_pos) in
    (* We are iterating from back to front... *)
    let cont2_opt = List.fold_left (fun acc cur -> (
      let (e, s, pos) = cur in
      let pos = t_of_lexing_position pos in
      match acc with
      | None -> Some (MiddleStringInterp (e, s, end_part, pos))
      | Some prev -> Some (MiddleStringInterp (e, s, prev, pos))
    )) None cont in
    let (ss, ss_pos) = ss in
    let ss_pos = t_of_lexing_position ss_pos in
    String (StartStringInterp (ss, Option.get cont2_opt, ss_pos), ss_pos)
  }
  | s = STRING_FULL {
    let (s, pos) = s in
    let pos = Sloth_common.Position.t_of_lexing_position pos in
    String (FullString (s, pos), pos)
  }
  | i = ID {
    let (i, pos) = i in
    let pos = Sloth_common.Position.t_of_lexing_position pos in
    IdRef (i, pos)
  }
  | i = PROTOTYPE {
    let (i, pos) = i in
    let pos = Sloth_common.Position.t_of_lexing_position pos in
    ProtoRef (i, pos)
  }
  | LPAREN; e = expr1; RPAREN { e }
  | h = hash_literals { h }
  ;

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
    let pos = Sloth_common.Position.t_of_lexing_position pos in
    List ([], pos)
  }
  (* expr_list will never end in COMMA *)
  | pos = LBRACKET; l = expr_list ; COMMA; RBRACKET {
    let pos = Sloth_common.Position.t_of_lexing_position pos in
    List (l, pos)
  }
  | pos = LBRACKET; l = expr_list ; RBRACKET {
    let pos = Sloth_common.Position.t_of_lexing_position pos in
    List (l, pos)
  }
  ;

hash_literals:
  | pos = LCURLY; RCURLY {
    let pos = Sloth_common.Position.t_of_lexing_position pos in
    HashMap ([], pos)
  }
  (* Allow single line literal without trailing comma *)
  | pos = LCURLY; k = expr1; COLON; v = expr1; RCURLY {
    let pos = Sloth_common.Position.t_of_lexing_position pos in
    HashMap ([(k, v)], pos)
  }
  | pos = LCURLY; p = hash_literal_pair; RCURLY {
    let pos = Sloth_common.Position.t_of_lexing_position pos in
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
    let pos = Sloth_common.Position.t_of_lexing_position pos in
    Subscript (e, sub, pos)
  }
  ;

conditional_continuation:
  | e = elif; pos = ELSE; b = block {
      let pos = Sloth_common.Position.t_of_lexing_position pos in
      let else_cont = ElseCont (b, pos) in
      match e with
      | IfCont {conditional; block; continuation=_; pos} -> IfCont { conditional; block; continuation=(Some else_cont); pos }
      | _ -> failwith "Unreachable"
  }
  | e = elif { e }
  | pos = ELSE; b = block {
      let pos = Sloth_common.Position.t_of_lexing_position pos in
      ElseCont (b, pos)
  }
  ;

elif:
  | prev = elif; pos = ELSE; IF; conditional = expr2; block = block {
    let pos = Sloth_common.Position.t_of_lexing_position pos in
    let cur = IfCont {conditional; block; continuation=None; pos} in
    match prev with
    | IfCont {conditional; block; continuation=_; pos} -> IfCont {conditional; block; continuation=(Some cur); pos}
    | _ -> failwith "Unreachable"
  }
  | pos = ELSE; IF; conditional = expr2; block = block {
    let pos = Sloth_common.Position.t_of_lexing_position pos in
    IfCont {conditional; block; continuation=None; pos}
  }

block:
  | LCURLY; RCURLY { [] }
  | LCURLY; s = stmts; RCURLY { s }

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
    let pos : Sloth_common.Position.t = Sloth_common.Position.t_of_lexing_position pos in
    let tuple = (i, pos) in
    [tuple]
  }
  | tl = parameter_list; COMMA; hd = ID {
    let (i, pos) = hd in
    let pos = Sloth_common.Position.t_of_lexing_position pos in
    (i, pos) :: tl
  }

%%

(* Footer *)
