exception Failure of string

type decl = private
  | FuncDecl of { name : string; parameters : string list; block : stmt list }
  | StmtDecl of stmt
[@@deriving sexp]

and stmt = private
  | LetStmt of string * expr
  | AssignStmt of string * expr
  | SubAssignStmt of { subscript : expr; value : expr }
  | ExprStmt of expr
  | ForLoop of stmt * expr * stmt * stmt list
[@@deriving sexp]

and expr = private
  | Num of float * Sloth_common.Position.t
  | Bool of bool
  | Null
  | String of string_parts list
  | List of expr list
  | HashMap of (expr * expr) list
  | Subscript of expr * expr
  | Binary of operator * expr * expr
  | IdRef of string
  | FuncInvoc of expr * expr list
  | MethodInvoc of { receiver : expr; target : string; args : expr list }
  | FuncExpr of { parameters : string list; block : stmt list }
  | IfExpr of cond_cont
[@@deriving sexp]

and string_parts =
  | FullString of string
  | StartStringInterp of string
  | MiddleStringInterp of string
  | EndStringInterp of string
  | ExpressionStringInterp of expr
[@@deriving sexp]

and cond_cont = private
  | IfCont of {
      conditional : expr;
      block : stmt list;
      continuation : cond_cont option;
    }
  | ElseCont of stmt list
[@@deriving sexp]

and operator = private Add [@@deriving sexp]

val optimize_prog : Environment.t -> Ast.decl list -> Environment.t * decl list
val prog_to_str : decl list -> string
