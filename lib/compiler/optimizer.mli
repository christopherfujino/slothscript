type decl = private
  | FuncDecl of {
      name : string;
      parameters : (string * (Sloth_common.Position.t[@sexp.opaque])) list;
      block : stmt list;
      pos : (Sloth_common.Position.t[@sexp.opaque]);
    }
  | StmtDecl of stmt
[@@deriving sexp]

and stmt =
  | ExprStmt of expr
  | BreakingStmt of Ast.breaking_type * expr option * (Sloth_common.Position.t[@sexp.opaque])
[@@deriving sexp]

and expr =
  | Num of float * (Sloth_common.Position.t[@sexp.opaque])
  | Bool of bool * (Sloth_common.Position.t[@sexp.opaque])
  | Null of (Sloth_common.Position.t[@sexp.opaque])
  | String of string_parts list * (Sloth_common.Position.t[@sexp.opaque])
  | List of expr list * (Sloth_common.Position.t[@sexp.opaque])
  | HashMap of (expr * expr) list * (Sloth_common.Position.t[@sexp.opaque])
  | Subscript of expr * expr * (Sloth_common.Position.t[@sexp.opaque])
  | IdRef of string * (Sloth_common.Position.t[@sexp.opaque])
  | ContextId of string * (Sloth_common.Position.t[@sexp.opaque])
  | Equality of expr * expr * bool * (Sloth_common.Position.t[@sexp.opaque])
  | Binary of expr * expr * Ast.operator * (Sloth_common.Position.t[@sexp.opaque])
  | FuncInvoc of expr * expr list * (Sloth_common.Position.t[@sexp.opaque])
  | MethodInvoc of {
      receiver : expr;
      target : string;
      args : expr list;
      pos : (Sloth_common.Position.t[@sexp.opaque]);
    }
  | FuncExpr of {
      parameters : (string * (Sloth_common.Position.t[@sexp.opaque])) list;
      block : stmt list;
      pos : (Sloth_common.Position.t[@sexp.opaque]);
    }
  | IfExpr of cond_cont * (Sloth_common.Position.t[@sexp.opaque])
  | UnaryExpr of {
      target : expr;
      pos : (Sloth_common.Position.t[@sexp.opaque]);
      operator : Ast.operator;
    }
  | DoBlock of stmt list * (Sloth_common.Position.t[@sexp.opaque])
  | ObjDeref of expr * string * (Sloth_common.Position.t[@sexp.opaque])
  | LetExpr of string * expr * (Sloth_common.Position.t[@sexp.opaque])
  | AssignExpr of string * expr * (Sloth_common.Position.t[@sexp.opaque])
  | SubAssignExpr of {
      subscript : expr;
      value : expr;
      pos : (Sloth_common.Position.t[@sexp.opaque]);
    }
  | ForLoop of expr * expr * expr * stmt list * (Sloth_common.Position.t[@sexp.opaque])
  | ForInLoop of {
      iterator_name : string;
      iteratee : expr;
      block : stmt list;
      pos : (Sloth_common.Position.t[@sexp.opaque]);
    }
  | WithExpr of (string * expr) list * stmt list * (Sloth_common.Position.t[@sexp.opaque])
[@@deriving sexp]

and string_parts =
  | FullString of string * (Sloth_common.Position.t[@sexp.opaque])
  | StartStringInterp of string * (Sloth_common.Position.t[@sexp.opaque])
  | MiddleStringInterp of string * (Sloth_common.Position.t[@sexp.opaque])
  | EndStringInterp of string * (Sloth_common.Position.t[@sexp.opaque])
  | ExpressionStringInterp of expr
[@@deriving sexp]

and cond_cont = private
  | IfCont of {
      conditional : expr;
      block : stmt list;
      continuation : cond_cont option;
      pos : (Sloth_common.Position.t[@sexp.opaque]);
    }
  | ElseCont of stmt list * (Sloth_common.Position.t[@sexp.opaque])
[@@deriving sexp]

val optimize_prog : Environment.t -> Ast.decl list -> Environment.t * decl list
val prog_to_str : decl list -> string
