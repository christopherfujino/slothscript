open Core

type func_stmt_t = {
  name : string;
  parameters : string list;
  block : stmt list;
}

and prog = stmt list [@@deriving sexp]

and stmt =
  | LetStmt of string * expr
  | ExprStmt of expr
  | FuncStmt of func_stmt_t
[@@deriving sexp]

and expr =
  | Num of float
  | Bool of bool
  | String of string
  | Binary of operator * expr * expr
  | IdRef of string
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

let prog_to_str stmts = sexp_of_prog stmts |> Sexp.to_string
