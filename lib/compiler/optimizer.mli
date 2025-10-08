type decl = private
  | FuncDecl of {
      name : string;
      parameters : (string * (Lexing.position[@sexp.opaque])) list;
      block : stmt list;
      pos : (Lexing.position[@sexp.opaque]);
    }
  | StmtDecl of stmt
[@@deriving sexp]

and stmt =
  | ExprStmt of expr
  | BreakingStmt of
      Ast.breaking_type * expr option * (Lexing.position[@sexp.opaque])
[@@deriving sexp]

and expr =
  | Num of float * (Lexing.position[@sexp.opaque])
  | Bool of bool * (Lexing.position[@sexp.opaque])
  | Null of (Lexing.position[@sexp.opaque])
  | String of string_parts list * (Lexing.position[@sexp.opaque])
  | List of expr list * (Lexing.position[@sexp.opaque])
  | HashMap of (expr * expr) list * (Lexing.position[@sexp.opaque])
  | Subscript of expr * expr * (Lexing.position[@sexp.opaque])
  | IdRef of string * (Lexing.position[@sexp.opaque])
  | ContextId of string * (Lexing.position[@sexp.opaque])
  | Equality of expr * expr * bool * (Lexing.position[@sexp.opaque])
  | Binary of expr * expr * Ast.operator * (Lexing.position[@sexp.opaque])
  | FuncInvoc of expr * expr list * (Lexing.position[@sexp.opaque])
  | MethodInvoc of {
      receiver : expr;
      target : string;
      args : expr list;
      pos : (Lexing.position[@sexp.opaque]);
    }
  | FuncExpr of {
      parameters : (string * (Lexing.position[@sexp.opaque])) list;
      block : stmt list;
      pos : (Lexing.position[@sexp.opaque]);
    }
  | IfExpr of cond_cont * (Lexing.position[@sexp.opaque])
  | UnaryExpr of {
      target : expr;
      pos : (Lexing.position[@sexp.opaque]);
      operator : Ast.operator;
    }
  | DoBlock of stmt list * (Lexing.position[@sexp.opaque])
  | ObjDeref of expr * string * (Lexing.position[@sexp.opaque])
  | LetExpr of string * expr * (Lexing.position[@sexp.opaque])
  | AssignExpr of string * expr * (Lexing.position[@sexp.opaque])
  | SubAssignExpr of {
      subscript : expr;
      value : expr;
      pos : (Lexing.position[@sexp.opaque]);
    }
  | DerefAssign of {
      receiver : expr;
      name : string;
      value : expr;
      pos : (Lexing.position[@sexp.opaque]);
    }
  | ForLoop of expr * expr * expr * stmt list * (Lexing.position[@sexp.opaque])
  | ForInLoop of {
      iterator_name : string;
      iteratee : expr;
      block : stmt list;
      pos : (Lexing.position[@sexp.opaque]);
    }
  | WithExpr of
      (string * expr) list * stmt list * (Lexing.position[@sexp.opaque])
  | CatchExpr of {
      subject : expr;
      capture : string;
      catch : expr;
      pos : (Lexing.position[@sexp.opaque]);
    }
[@@deriving sexp]

and string_parts =
  | FullString of string * (Lexing.position[@sexp.opaque])
  | StartStringInterp of string * (Lexing.position[@sexp.opaque])
  | MiddleStringInterp of string * (Lexing.position[@sexp.opaque])
  | EndStringInterp of string * (Lexing.position[@sexp.opaque])
  | ExpressionStringInterp of expr
[@@deriving sexp]

and cond_cont = private
  | IfCont of {
      conditional : expr;
      block : stmt list;
      continuation : cond_cont option;
      pos : (Lexing.position[@sexp.opaque]);
    }
  | ElseCont of stmt list * (Lexing.position[@sexp.opaque])
[@@deriving sexp]

val optimize_prog : Environment.t -> Ast.decl list -> Environment.t * decl list
val prog_to_str : decl list -> string
