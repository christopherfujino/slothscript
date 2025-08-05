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
  | SubAssignStmt of { subscript : expr; value : expr }
  | ExprStmt of expr
  | ForLoop of stmt * expr * stmt * stmt list
[@@deriving sexp]

and expr =
  | Num of float * Sloth_common.Position.t
  | Bool of bool
  | Null
  | String of string_parts
  | List of expr list
  | HashMap of (expr * expr) list
  | Subscript of expr * expr
  (* TODO delete *)
  | Binary of operator * expr * expr
  | IdRef of string
  | FuncInvoc of expr * expr list
  | MethodInvoc of { receiver : expr; target : string; args : expr list }
  | FuncExpr of func_expr_t
  | IfExpr of cond_cont
[@@deriving sexp]

and string_parts =
  | FullString of string
  | StartStringInterp of string * string_continuation
[@@deriving sexp]

and string_continuation =
  | MiddleStringInterp of expr * string * string_continuation
  | EndStringInterp of expr * string
[@@deriving sexp]

and cond_cont =
  | IfCont of {
      conditional : expr;
      block : stmt list;
      continuation : cond_cont option;
    }
  | ElseCont of stmt list

and operator = Add [@@deriving sexp]

let num_of_expr expr =
  match expr with
  | Num (f, _) -> f
  | _ ->
      let sexp = sexp_of_expr expr in
      let s = Sexp.to_string sexp in
      let msg = Printf.sprintf "Cast error! %s" s in
      failwith msg
