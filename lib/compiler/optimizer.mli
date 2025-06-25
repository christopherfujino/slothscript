exception Failure of string

type decl =
  | FuncDecl of { name : string; parameters : string list; block : stmt list }
  | StmtDecl of stmt
[@@deriving sexp]

and stmt =
  | LetStmt of string * expr
  | AssignStmt of string * expr
  | ExprStmt of expr
[@@deriving sexp]

and expr =
  | Num of float
  | Bool of bool
  | String of string
  | Binary of operator * expr * expr
  | IdRef of string
  | FuncInvoc of expr * expr list
  | FuncExpr of { parameters : string list; block : stmt list }
[@@deriving sexp]

and operator = Add
[@@deriving sexp]

val optimize_prog : Ast.decl list -> decl list
val prog_to_str : decl list -> string
