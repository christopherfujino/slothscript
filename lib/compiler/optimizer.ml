open Core

exception Failure of string

let failure msg pos =
  let pos_msg = Sloth_common.Position.string_of_t pos in
  let msg2 = Printf.sprintf "[%s] Optimizer error: %s" pos_msg msg in
  raise (Failure msg2)

type prog = decl list

and decl =
  | FuncDecl of {
      name : string;
      parameters : (string * Sloth_common.Position.t) list;
      block : stmt list;
    }
  | StmtDecl of stmt

and stmt =
  | LetStmt of string * expr
  | AssignStmt of string * expr
  | SubAssignStmt of { subscript : expr; value : expr }
  | ExprStmt of expr
  | ForLoop of stmt * expr * stmt * stmt list
[@@deriving sexp]

and expr =
  | Num of float * Sloth_common.Position.t
  | Bool of bool * Sloth_common.Position.t
  | Null of Sloth_common.Position.t
  | String of string_parts list * Sloth_common.Position.t
  | List of expr list * Sloth_common.Position.t
  | HashMap of (expr * expr) list * Sloth_common.Position.t
  | Subscript of expr * expr * Sloth_common.Position.t
  | IdRef of string * Sloth_common.Position.t
  | FuncInvoc of expr * expr list * Sloth_common.Position.t
  | MethodInvoc of {
      receiver : expr;
      target : string;
      args : expr list;
      pos : Sloth_common.Position.t;
    }
  | FuncExpr of {
      parameters : (string * Sloth_common.Position.t) list;
      block : stmt list;
      pos : Sloth_common.Position.t;
    }
  | IfExpr of cond_cont * Sloth_common.Position.t
[@@deriving sexp]

and string_parts =
  | FullString of string * Sloth_common.Position.t
  | StartStringInterp of string * Sloth_common.Position.t
  | MiddleStringInterp of string * Sloth_common.Position.t
  | EndStringInterp of string * Sloth_common.Position.t
  | ExpressionStringInterp of expr
[@@deriving sexp]

and cond_cont =
  | IfCont of {
      conditional : expr;
      block : stmt list;
      continuation : cond_cont option;
      pos : Sloth_common.Position.t;
    }
  | ElseCont of stmt list * Sloth_common.Position.t

and operator = Add [@@deriving sexp]

let rec optimize_prog env prog =
  let prog2 = List.rev prog in
  (* TODO lift this so it can be shared across calls in the REPL
  let env = Environment.create () |> Stdlib_stubs.populate in
  *)
  let f =
   fun acc decl ->
    let env1, already_opt_decls = acc in
    let env2, opt_decl = optimize_decl env1 decl in
    (env2, opt_decl :: already_opt_decls)
  in
  let final_env, decls = List.fold_left prog2 ~f ~init:(env, []) in
  (final_env, List.rev decls)

and optimize_decl env decl : Environment.t * decl =
  match decl with
  | Ast.FuncDecl { name; parameters; block } ->
      let parameters2 = List.rev parameters in
      (* Bind name to the env, both for recursion and return value *)
      let env2 = Environment.bind env name in
      let env3 = Environment.push_empty env2 in
      let env4 =
        List.fold_left parameters2 ~init:env3 ~f:(fun env param ->
            let param, _ = param in
            Environment.bind env param)
      in
      let block2 = optimize_block env4 block in
      (env2, FuncDecl { name; parameters = parameters2; block = block2 })
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
  | SubAssignStmt { subscript; value } ->
      let subscript = optimize_expr env subscript in
      let value = optimize_expr env value in
      (env, SubAssignStmt { subscript; value })
  | ExprStmt expr ->
      let e = optimize_expr env expr in
      (env, ExprStmt e)
  | ForLoop (init, comp, inc, block) ->
      let env2, init' = optimize_stmt env init in
      let comp' = optimize_expr env2 comp in
      let env3, inc' = optimize_stmt env2 inc in
      let block' = optimize_block env3 block in
      (env, ForLoop (init', comp', inc', block'))

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

and optimize_expr (env : Environment.t) (e : Ast.expr) : expr =
  match e with
  | Num (f, pos) -> Num (f, pos)
  | Bool (b, pos) -> Bool (b, pos)
  | Null pos -> Null pos
  | String (s, pos) -> optimize_string env s pos
  | List (els, pos) ->
      let rev_opt_els = List.rev els |> List.map ~f:(optimize_expr env) in
      List (rev_opt_els, pos)
  | HashMap (kvps, pos) ->
      (* TODO use a Hashtbl? *)
      let kvps' =
        List.map kvps ~f:(fun (k, v) ->
            (optimize_expr env k, optimize_expr env v))
      in
      HashMap (kvps', pos)
  | Subscript (receiver, sub, pos) ->
      let receiver' = optimize_expr env receiver in
      let sub' = optimize_expr env sub in
      Subscript (receiver', sub', pos)
  | FuncInvoc (receiver, args, pos) ->
      let rev_args = List.rev args in
      let rev_mapped_args = List.map rev_args ~f:(optimize_expr env) in
      FuncInvoc (optimize_expr env receiver, rev_mapped_args, pos)
  | IdRef (name, pos) -> (
      let name_opt = Environment.find env name in
      match name_opt with
      | None ->
          let msg = Printf.sprintf "Undeclared identifier %s" name in
          failure msg pos
      | Some _ -> IdRef (name, pos))
  | FuncExpr { parameters; block; pos } ->
      let parameters2 = List.rev parameters in
      let env2 = Environment.push_empty env in
      let env3 =
        List.fold_left parameters2 ~init:env2 ~f:(fun env param ->
            let param, _ = param in
            Environment.bind env param)
      in
      let block2 = optimize_block env3 block in
      FuncExpr { parameters = parameters2; block = block2; pos }
  | IfExpr (cond_cont, pos) -> IfExpr (optimize_continuation env cond_cont, pos)
  | MethodInvoc { target; receiver; args; pos } ->
      let optim_receiver = optimize_expr env receiver in
      let optim_args = List.rev args |> List.map ~f:(optimize_expr env) in
      MethodInvoc { target; receiver = optim_receiver; args = optim_args; pos }

and optimize_string env s pos =
  String
    ( (match s with
      | FullString (s', pos) -> [ FullString (s', pos) ]
      | StartStringInterp (s', cont1, pos) ->
          let cont2 = optimize_string_continuation env cont1 in
          StartStringInterp (s', pos) :: cont2),
      pos )

and optimize_string_continuation env cont =
  match cont with
  | MiddleStringInterp (e, s, cont2, pos) ->
      let e2 = optimize_expr env e in
      let cont3 = optimize_string_continuation env cont2 in
      ExpressionStringInterp e2 :: MiddleStringInterp (s, pos) :: cont3
  | EndStringInterp (e, s, pos) ->
      [ ExpressionStringInterp (optimize_expr env e); EndStringInterp (s, pos) ]

and optimize_continuation env c =
  match c with
  | Ast.IfCont { conditional; block; continuation; pos } ->
      let conditional = optimize_expr env conditional in
      let env2 = Environment.push_empty env in
      let block = optimize_block env2 block in
      let continuation =
        Option.map continuation ~f:(optimize_continuation env)
      in
      IfCont { conditional; block; continuation; pos }
  | Ast.ElseCont (stmts, pos) ->
      let env2 = Environment.push_empty env in
      let optimized_stmts = optimize_block env2 stmts in
      ElseCont (optimized_stmts, pos)

let prog_to_str stmts = sexp_of_prog stmts |> Sexp.to_string
