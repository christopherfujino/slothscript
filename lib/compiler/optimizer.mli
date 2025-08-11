exception Failure of string

type decl = private
  | FuncDecl of {
      name : string;
      parameters : (string * Sloth_common.Position.t) list;
      block : stmt list;
      pos : Sloth_common.Position.t;
    }
  | StmtDecl of stmt
[@@deriving sexp]

and stmt =
  | LetStmt of string * expr * Sloth_common.Position.t
  | AssignStmt of string * expr * Sloth_common.Position.t
  | SubAssignStmt of {
      subscript : expr;
      value : expr;
      pos : Sloth_common.Position.t;
    }
  | ExprStmt of expr
  | ForLoop of stmt * expr * stmt * stmt list * Sloth_common.Position.t
[@@deriving sexp]

and expr = private
  | Num of float * Sloth_common.Position.t
  | Bool of bool * Sloth_common.Position.t
  | Null of Sloth_common.Position.t
  | String of string_parts list * Sloth_common.Position.t
  | List of expr list * Sloth_common.Position.t
  | HashMap of (expr * expr) list * Sloth_common.Position.t
  | Subscript of expr * expr * Sloth_common.Position.t
  | IdRef of string * Sloth_common.Position.t
  | Equality of expr * expr * bool * Sloth_common.Position.t
  | FuncInvoc of expr * expr list * Sloth_common.Position.t
  | MethodInvoc of {
      receiver : expr;
      target : string;
      args : expr list;
      pos : Sloth_common.Position.t;
    }
  | FuncExpr of {
      parameters : (string * Sloth_common.Position.t) list;
      block : stmt list;
      pos : Sloth_common.Position.t;
    }
  | IfExpr of cond_cont * Sloth_common.Position.t
  | UnaryExpr of {
      target : expr;
      pos : Sloth_common.Position.t;
      is_prefix : bool;
      operator : Ast.operator;
    }
[@@deriving sexp]

and string_parts =
  | FullString of string * Sloth_common.Position.t
  | StartStringInterp of string * Sloth_common.Position.t
  | MiddleStringInterp of string * Sloth_common.Position.t
  | EndStringInterp of string * Sloth_common.Position.t
  | ExpressionStringInterp of expr
[@@deriving sexp]

and cond_cont = private
  | IfCont of {
      conditional : expr;
      block : stmt list;
      continuation : cond_cont option;
      pos : Sloth_common.Position.t;
    }
  | ElseCont of stmt list * Sloth_common.Position.t
[@@deriving sexp]

val optimize_prog : Environment.t -> Ast.decl list -> Environment.t * decl list
val prog_to_str : decl list -> string
