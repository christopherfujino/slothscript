open Core

type func_stmt_t = {
  name : string;
  parameters : (string * (Lexing.position[@sexp.opaque])) list;
  block : stmt list;
  pos : (Lexing.position[@sexp.opaque]);
}

and func_expr_t = {
  parameters : (string * (Lexing.position[@sexp.opaque])) list;
  block : stmt list;
  pos : (Lexing.position[@sexp.opaque]);
}

and prog = stmt list [@@deriving sexp]
and decl = FuncDecl of func_stmt_t | StmtDecl of stmt

and stmt =
  (* No position *)
  | ExprStmt of expr
  | BreakingStmt of
      breaking_type * expr option * (Lexing.position[@sexp.opaque])
[@@deriving sexp]

and breaking_type =
  | Return
  | Break
  | Continue
  | Error of string (* TODO add an enum type *)
  | Exit of int
[@@deriving sexp]
(* TODO make an interpreter type to store the runtime data *)

and expr =
  | Num of float * (Lexing.position[@sexp.opaque])
  | Bool of bool * (Lexing.position[@sexp.opaque])
  | Null of (Lexing.position[@sexp.opaque])
  | String of string_parts * (Lexing.position[@sexp.opaque])
  | List of expr list * (Lexing.position[@sexp.opaque])
  | HashMap of (expr * expr) list * (Lexing.position[@sexp.opaque])
  | Subscript of expr * expr * (Lexing.position[@sexp.opaque])
  | IdRef of string * (Lexing.position[@sexp.opaque])
  | ContextId of string * (Lexing.position[@sexp.opaque])
  | ProtoRef of string * (Lexing.position[@sexp.opaque])
  | FuncInvoc of expr * expr list * (Lexing.position[@sexp.opaque])
  (* Migrate to Binary *)
  | Equality of expr * expr * bool * (Lexing.position[@sexp.opaque])
  | Binary of expr * expr * operator * (Lexing.position[@sexp.opaque])
  | ObjDeref of expr * string * (Lexing.position[@sexp.opaque])
  | MethodInvoc of {
      receiver : expr;
      target : string;
      args : expr list;
      pos : (Lexing.position[@sexp.opaque]);
    }
  | FuncExpr of func_expr_t
  | IfExpr of cond_cont * (Lexing.position[@sexp.opaque])
  | UnaryExpr of {
      target : expr;
      pos : (Lexing.position[@sexp.opaque]);
      operator : operator;
    }
  | DoBlock of stmt list * (Lexing.position[@sexp.opaque])
  | LetExpr of string * expr * (Lexing.position[@sexp.opaque])
  | AssignExpr of string * expr * (Lexing.position[@sexp.opaque])
  | SubAssignExpr of {
      subscript : expr;
      value : expr;
      pos : (Lexing.position[@sexp.opaque]);
    }
  | ForLoop of expr * expr * expr * stmt list * (Lexing.position[@sexp.opaque])
  | ForInLoop of {
      iterator_name : string;
      iteratee : expr;
      block : stmt list;
      pos : (Lexing.position[@sexp.opaque]);
    }
  | WithExpr of
      (string * expr) list * stmt list * (Lexing.position[@sexp.opaque])
[@@deriving sexp]

and string_parts =
  | FullString of string * (Lexing.position[@sexp.opaque])
  | StartStringInterp of
      string * string_continuation * (Lexing.position[@sexp.opaque])
[@@deriving sexp]

and string_continuation =
  | MiddleStringInterp of
      expr * string * string_continuation * (Lexing.position[@sexp.opaque])
  | EndStringInterp of expr * string * (Lexing.position[@sexp.opaque])
[@@deriving sexp]

and cond_cont =
  | IfCont of {
      conditional : expr;
      block : stmt list;
      continuation : cond_cont option;
      pos : (Lexing.position[@sexp.opaque]);
    }
  | ElseCont of stmt list * (Lexing.position[@sexp.opaque])

and operator =
  | Plus
  | Minus
  | Product
  | Divide
  | Modulo
  | Pipe
  | Less
  | Greater
  | Leq
  | Geq
  | Bang
  | Ampersand
  | And
  | Or
  | Not
  | LeftArrow
  | RightArrow
[@@deriving sexp]

let num_of_expr expr =
  match expr with
  | Num (f, _) -> f
  | _ ->
      let sexp = sexp_of_expr expr in
      let s = Sexp.to_string sexp in
      let msg = Printf.sprintf "Cast error! %s" s in
      failwith msg
