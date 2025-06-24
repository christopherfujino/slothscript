open Core

exception Failure of string

type expr =
  | Num of float
  | Bool of bool
  | String of string
  (* TODO this should be infix invoc expression, storing a lexeme string *)
  | Binary of operator * expr * expr
  | IdRef of string
  | FuncInvoc of string * expr list
[@@deriving sexp]

and operator = Add [@@deriving sexp]

and stmt =
  | LetStmt of string * expr
  | AssignStmt of string * expr
  | ExprStmt of expr
  | FuncStmt of { name : string; parameters : string list; block : stmt list }
[@@deriving sexp]

type prog = stmt list [@@deriving sexp]

let rec optimize_prog prog =
  let env = Environment.create () in
  optimize_block env prog

and optimize_stmt env stmts : Environment.t * stmt =
  let open Ast in
  match stmts with
  | LetStmt (name, expr) ->
      let e = optimize_expr env expr in
      let env2 = Environment.bind env name in
      (env2, LetStmt (name, e))
  | AssignStmt (name, expr) ->
      let e = optimize_expr env expr in
      (env, AssignStmt (name, e))
  | ExprStmt expr ->
      let e = optimize_expr env expr in
      (env, ExprStmt e)
  | FuncStmt { name; parameters; block } ->
      let parameters = List.rev parameters in
      (* TODO will need to add `name` to env to support recursion *)
      let env2 = Environment.push_empty env in
      let env3 =
        List.fold_left parameters ~init:env2 ~f:(fun env param ->
            Environment.bind env param)
      in
      let block2 = optimize_block env3 block in
      let env4 = Environment.bind env name in
      (env4, FuncStmt { name; parameters; block = block2 })

(** You must push a new frame to the env first. *)
and optimize_block env rev_stmts =
  let stmts = List.rev rev_stmts in
  let cb =
   fun acc cur ->
    let env, stmts = acc in
    let env2, stmt = optimize_stmt env cur in
    (env2, stmt :: stmts)
  in

  let _, rev_stmts2 = List.fold_left ~init:(env, []) ~f:cb stmts in
  List.rev rev_stmts2

and optimize_operator (o : Ast.operator) : operator = match o with Add -> Add

and optimize_expr (env : Environment.t) (e : Ast.expr) : expr =
  let open Ast in
  match e with
  | Num f -> Num f
  | Bool b -> Bool b
  | String s -> String s
  | Binary (o, e1, e2) ->
      let e1 = optimize_expr env e1 in
      let e2 = optimize_expr env e2 in
      Binary (optimize_operator o, e1, e2)
  | FuncInvoc (name, args) ->
      let rev_args = List.rev args in
      let rev_mapped_args = List.map rev_args ~f:(optimize_expr env) in
      FuncInvoc (name, rev_mapped_args)
  | IdRef name -> (
      let name_opt = Environment.find env name in
      match name_opt with
      | None ->
          let msg = Printf.sprintf "Undeclared identifier %s" name in
          raise (Failure msg)
      | Some _ -> IdRef name)

let prog_to_str stmts = sexp_of_prog stmts |> Sexp.to_string
