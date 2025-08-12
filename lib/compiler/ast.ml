open Core

type func_stmt_t = {
  name : string;
  parameters : (string * Sloth_common.Position.t) list;
  block : stmt list;
  pos : Sloth_common.Position.t;
}

and func_expr_t = {
  parameters : (string * Sloth_common.Position.t) list;
  block : stmt list;
  pos : Sloth_common.Position.t;
}

and prog = stmt list [@@deriving sexp]
and decl = FuncDecl of func_stmt_t | StmtDecl of stmt

and stmt =
  | LetStmt of string * expr * Sloth_common.Position.t
  | AssignStmt of string * expr * Sloth_common.Position.t
  | SubAssignStmt of {
      subscript : expr;
      value : expr;
      pos : Sloth_common.Position.t;
    }
  (* No position *)
  | ExprStmt of expr
  | ForLoop of stmt * expr * stmt * stmt list * Sloth_common.Position.t
  | ForInLoop of {
      iterator_name : string;
      iteratee : expr;
      block : stmt list;
      pos : Sloth_common.Position.t;
    }
[@@deriving sexp]

and expr =
  | Num of float * Sloth_common.Position.t
  | Bool of bool * Sloth_common.Position.t
  | Null of Sloth_common.Position.t
  | String of string_parts * Sloth_common.Position.t
  | List of expr list * Sloth_common.Position.t
  | HashMap of (expr * expr) list * Sloth_common.Position.t
  | Subscript of expr * expr * Sloth_common.Position.t
  | IdRef of string * Sloth_common.Position.t
  | FuncInvoc of expr * expr list * Sloth_common.Position.t
  | Equality of expr * expr * bool * Sloth_common.Position.t
  | MethodInvoc of {
      receiver : expr;
      target : string;
      args : expr list;
      pos : Sloth_common.Position.t;
    }
  | FuncExpr of func_expr_t
  | IfExpr of cond_cont * Sloth_common.Position.t
  | UnaryExpr of {
      target : expr;
      pos : Sloth_common.Position.t;
      is_prefix : bool;
      operator : operator;
    }
  | DoBlock of stmt list * Sloth_common.Position.t
[@@deriving sexp]

and string_parts =
  | FullString of string * Sloth_common.Position.t
  | StartStringInterp of string * string_continuation * Sloth_common.Position.t
[@@deriving sexp]

and string_continuation =
  | MiddleStringInterp of
      expr * string * string_continuation * Sloth_common.Position.t
  | EndStringInterp of expr * string * Sloth_common.Position.t
[@@deriving sexp]

and cond_cont =
  | IfCont of {
      conditional : expr;
      block : stmt list;
      continuation : cond_cont option;
      pos : Sloth_common.Position.t;
    }
  | ElseCont of stmt list * Sloth_common.Position.t

and operator = Bang [@@deriving sexp]

let num_of_expr expr =
  match expr with
  | Num (f, _) -> f
  | _ ->
      let sexp = sexp_of_expr expr in
      let s = Sexp.to_string sexp in
      let msg = Printf.sprintf "Cast error! %s" s in
      failwith msg
