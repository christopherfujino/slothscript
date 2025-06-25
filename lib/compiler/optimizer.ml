open Core

exception Failure of string

type prog = decl list

and decl =
  | FuncDecl of { name : string; parameters : string list; block : stmt list }
  | StmtDecl of stmt

and stmt =
  | LetStmt of string * expr
  | AssignStmt of string * expr
  | ExprStmt of expr
[@@deriving sexp]

and expr =
  | Num of float
  | Bool of bool
  | String of string
  (* TODO this should be infix invoc expression, storing a lexeme string *)
  | Binary of operator * expr * expr
  | IdRef of string
  | FuncInvoc of expr * expr list
  | FuncExpr of { parameters : string list; block : stmt list }
  | IfExpr of { conditional : expr; block : stmt list }
[@@deriving sexp]

and operator = Add [@@deriving sexp]

let rec optimize_prog prog =
  let prog2 = List.rev prog in
  let env = Environment.create () |> Stdlib_stubs.populate in
  let f =
   fun acc decl ->
    let env1, already_opt_decls = acc in
    let env2, opt_decl = optimize_decl env1 decl in
    (env2, opt_decl :: already_opt_decls)
  in
  let _, decls = List.fold_left prog2 ~f ~init:(env, []) in
  List.rev decls

and optimize_decl env decl : Environment.t * decl =
  match decl with
  | Ast.FuncDecl { name; parameters; block } ->
      let parameters2 = List.rev parameters in
      (* TODO will need to add `name` to env to support recursion *)
      let env2 = Environment.push_empty env in
      let env3 =
        List.fold_left parameters2 ~init:env2 ~f:(fun env param ->
            Environment.bind env param)
      in
      let block2 = optimize_block env3 block in
      let env4 = Environment.bind env name in
      (env4, FuncDecl { name; parameters = parameters2; block = block2 })
  | Ast.StmtDecl s ->
      let env2, stmt = optimize_stmt env s in
      (env2, StmtDecl stmt)

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
  | FuncInvoc (receiver, args) ->
      let rev_args = List.rev args in
      let rev_mapped_args = List.map rev_args ~f:(optimize_expr env) in
      FuncInvoc (optimize_expr env receiver, rev_mapped_args)
  | IdRef name -> (
      let name_opt = Environment.find env name in
      match name_opt with
      | None ->
          let msg = Printf.sprintf "Undeclared identifier %s" name in
          raise (Failure msg)
      | Some _ -> IdRef name)
  | FuncExpr { parameters; block } ->
      let parameters2 = List.rev parameters in
      let env2 = Environment.push_empty env in
      let env3 =
        List.fold_left parameters2 ~init:env2 ~f:(fun env param ->
            Environment.bind env param)
      in
      let block2 = optimize_block env3 block in
      FuncExpr { parameters = parameters2; block = block2 }
  | IfExpr { conditional; block } ->
      let optimized_conditional = optimize_expr env conditional in
      let optimized_block = optimize_block env block in
      IfExpr { conditional = optimized_conditional; block = optimized_block }

let prog_to_str stmts = sexp_of_prog stmts |> Sexp.to_string
