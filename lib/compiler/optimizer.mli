exception Failure of string

type decl = private
  | FuncDecl of { name : string; parameters : string list; block : stmt list }
  | StmtDecl of stmt
[@@deriving sexp]

and stmt = private
  | LetStmt of string * expr
  | AssignStmt of string * expr
  | ExprStmt of expr
[@@deriving sexp]

and expr = private
  | Num of float
  | Bool of bool
  | Null
  | String of string
  | List of expr list
  | Subscript of expr * expr
  | Binary of operator * expr * expr
  | IdRef of string
  | FuncInvoc of expr * expr list
  | MethodInvoc of { receiver : expr; target : string; args : expr list }
  | FuncExpr of { parameters : string list; block : stmt list }
  | ForLoop of stmt * expr * stmt * stmt list
  | IfExpr of cond_cont
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
