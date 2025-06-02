open Core

type expr =
  | Num of float
  | Bool of bool
  | String of string
  | Binary of operator * expr * expr
  | IdRef of string
  | FuncInvoc of string * expr list
[@@deriving sexp]

and operator = Add [@@deriving sexp]

and stmt =
  | LetStmt of string * expr
  | ExprStmt of expr
  | FuncStmt of { name : string; parameters : string list; block : stmt list }
[@@deriving sexp]

type prog = stmt list [@@deriving sexp]

let rec optimize_stmts prog =
  (* TODO Compiler.optimize should compile to IR. *)
  let prog = List.rev prog in
  List.map prog ~f:optimize_stmt

and optimize_stmt stmts : stmt =
  let open Ast in
  match stmts with
  | LetStmt (name, expr) ->
      let e = optimize_expr expr in
      LetStmt (name, e)
  | ExprStmt expr ->
      let e = optimize_expr expr in
      ExprStmt e
  | FuncStmt { name; parameters; block } ->
      let parameters = List.rev parameters in
      let block = List.rev block |> List.map ~f:optimize_stmt in
      FuncStmt { name; parameters; block }

and optimize_operator (o : Ast.operator) : operator = match o with Add -> Add

and optimize_expr e : expr =
  let open Ast in
  match e with
  | Num f -> Num f
  | Bool b -> Bool b
  | String s -> String s
  | Binary (o, e1, e2) ->
      let e1 = optimize_expr e1 in
      let e2 = optimize_expr e2 in
      Binary (optimize_operator o, e1, e2)
  | FuncInvoc (name, args) ->
      let rev_args = List.rev args in
      let rev_mapped_args = List.map rev_args ~f:optimize_expr in
      FuncInvoc (name, rev_mapped_args)
  | IdRef name -> IdRef name

let prog_to_str stmts = sexp_of_prog stmts |> Sexp.to_string
