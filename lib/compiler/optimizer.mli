type expr =
  | Num of float
  | Bool of bool
  | String of string
  | Binary of operator * expr * expr
  | IdRef of string
  | FuncInvoc of string * expr list

and operator = Add

and stmt =
  | LetStmt of string * expr
  | AssignStmt of string * expr
  | ExprStmt of expr
  | FuncStmt of { name : string; parameters : string list; block : stmt list }

val optimize_stmts : Ast.stmt list -> stmt list
val optimize_stmt : Ast.stmt -> stmt
val optimize_expr : Ast.expr -> expr
val optimize_operator : Ast.operator -> operator
val prog_to_str : stmt list -> string
