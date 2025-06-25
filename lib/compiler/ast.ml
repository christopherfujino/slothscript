open Core

type func_stmt_t = {
  name : string;
  parameters : string list;
  block : stmt list;
}

and func_expr_t = { parameters : string list; block : stmt list }
and prog = stmt list [@@deriving sexp]
and decl = FuncDecl of func_stmt_t | StmtDecl of stmt

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
  | FuncExpr of func_expr_t
[@@deriving sexp]

and operator = Add [@@deriving sexp]

let num_of_expr expr =
  match expr with
  | Num f -> f
  | _ ->
      let sexp = sexp_of_expr expr in
      let s = Sexp.to_string sexp in
      let msg = Printf.sprintf "Cast error! %s" s in
      failwith msg
