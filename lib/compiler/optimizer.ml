open Core

let fail ~env ~pos msg =
  let pos_msg = Sloth_common.Position.string_of_t pos in
  let msg2 =
    Printf.sprintf "[%s] Optimizer error\n\n%s\n%s" pos_msg
      (Sloth_common.Position.summarize pos (Environment.src env))
      msg
  in
  raise (Sloth_common.Common.CompileError msg2)

type prog = decl list

and decl =
  | FuncDecl of {
      name : string;
      parameters : (string * (Lexing.position[@sexp.opaque])) list;
      block : stmt list;
      pos : (Lexing.position[@sexp.opaque]);
    }
  | StmtDecl of stmt
[@@deriving sexp]

and stmt =
  | ExprStmt of expr
  | BreakingStmt of
      Ast.breaking_type * expr option * (Lexing.position[@sexp.opaque])
[@@deriving sexp]

and expr =
  | Num of float * (Lexing.position[@sexp.opaque])
  | Bool of bool * (Lexing.position[@sexp.opaque])
  | Null of (Lexing.position[@sexp.opaque])
  | String of string_parts list * (Lexing.position[@sexp.opaque])
  | List of expr list * (Lexing.position[@sexp.opaque])
  | HashMap of (expr * expr) list * (Lexing.position[@sexp.opaque])
  | Subscript of expr * expr * (Lexing.position[@sexp.opaque])
  | IdRef of string * (Lexing.position[@sexp.opaque])
  | ContextId of string * (Lexing.position[@sexp.opaque])
  | Equality of expr * expr * bool * (Lexing.position[@sexp.opaque])
  | Binary of expr * expr * Ast.operator * (Lexing.position[@sexp.opaque])
  | FuncInvoc of expr * expr list * (Lexing.position[@sexp.opaque])
  | MethodInvoc of {
      receiver : expr;
      target : string;
      args : expr list;
      pos : (Lexing.position[@sexp.opaque]);
    }
  | FuncExpr of {
      parameters : (string * (Lexing.position[@sexp.opaque])) list;
      block : stmt list;
      pos : (Lexing.position[@sexp.opaque]);
    }
  | IfExpr of cond_cont * (Lexing.position[@sexp.opaque])
  | UnaryExpr of {
      target : expr;
      pos : (Lexing.position[@sexp.opaque]);
      operator : Ast.operator;
    }
  | DoBlock of stmt list * (Lexing.position[@sexp.opaque])
  | ObjDeref of expr * string * (Lexing.position[@sexp.opaque])
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
  | StartStringInterp of string * (Lexing.position[@sexp.opaque])
  | MiddleStringInterp of string * (Lexing.position[@sexp.opaque])
  | EndStringInterp of string * (Lexing.position[@sexp.opaque])
  | ExpressionStringInterp of expr
[@@deriving sexp]

and cond_cont =
  | IfCont of {
      conditional : expr;
      block : stmt list;
      continuation : cond_cont option;
      pos : (Lexing.position[@sexp.opaque]);
    }
  | ElseCont of stmt list * (Lexing.position[@sexp.opaque])

let rec optimize_prog env prog =
  let prog2 = List.rev prog in
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
  | Ast.FuncDecl { name; parameters; block; pos } ->
      let parameters2 = List.rev parameters in
      (* Bind name to the env, both for recursion and return value *)
      let env2 =
        match Environment.bind env name with
        | None ->
            Printf.sprintf "The name %s has already been declared" name
            |> fail ~env ~pos
        | Some e -> e
      in
      let env3 = Environment.push_empty env2 in
      let env4 =
        List.fold_left parameters2 ~init:env3 ~f:(fun env param ->
            let param, _ = param in
            match Environment.bind env param with
            | None ->
                Printf.sprintf
                  "The name %s has already been declared in this scope" param
                |> fail ~env ~pos
            | Some e -> e)
      in
      let block2 = optimize_block env4 block in
      (env2, FuncDecl { name; parameters = parameters2; block = block2; pos })
  | Ast.StmtDecl s ->
      let env2, stmt = optimize_stmt env s in
      (env2, StmtDecl stmt)

and optimize_stmt env stmts : Environment.t * stmt =
  let open Ast in
  match stmts with
  | ExprStmt expr ->
      let env, e = optimize_expr env expr in
      (env, ExprStmt e)
  | BreakingStmt (breaking_t, expr_opt, pos) ->
      let env, expr_opt =
        match expr_opt with
        | None -> (env, None)
        | Some expr ->
            let env, expr = optimize_expr env expr in
            (env, Some expr)
      in
      (env, BreakingStmt (breaking_t, expr_opt, pos))

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

and optimize_expr (env : Environment.t) (e : Ast.expr) : Environment.t * expr =
  match e with
  | Num (f, pos) -> (env, Num (f, pos))
  | Bool (b, pos) -> (env, Bool (b, pos))
  | Null pos -> (env, Null pos)
  | String (s, pos) -> optimize_string env s pos
  | List (els, pos) ->
      let env, rev_opt_els =
        List.fold els ~init:(env, []) ~f:(fun (env, prev) el ->
            let env, expr = optimize_expr env el in
            (* This (correctly) reverses the elements *)
            (env, expr :: prev))
      in
      (env, List (rev_opt_els, pos))
  | HashMap (kvps, pos) ->
      (* TODO use a Hashtbl? *)
      let env, kvps' =
        List.fold kvps ~init:(env, []) ~f:(fun (env, prev) (k, v) ->
            let env, k = optimize_expr env k in
            let env, v = optimize_expr env v in
            (* This reverses, but it shouldn't matter *)
            (env, (k, v) :: prev))
      in
      (env, HashMap (kvps', pos))
  | Subscript (receiver, sub, pos) ->
      let env, receiver' = optimize_expr env receiver in
      let env, sub' = optimize_expr env sub in
      (env, Subscript (receiver', sub', pos))
  | Equality (lhs, rhs, is_equal, pos) ->
      let env, lhs = optimize_expr env lhs in
      let env, rhs = optimize_expr env rhs in
      (env, Equality (lhs, rhs, is_equal, pos))
  | Binary (lhs, rhs, op, pos) ->
      let env, lhs = optimize_expr env lhs in
      let env, rhs = optimize_expr env rhs in
      (env, Binary (lhs, rhs, op, pos))
  | FuncInvoc (receiver, args, pos) ->
      let env, receiver = optimize_expr env receiver in
      let env, args =
        List.fold args ~init:(env, []) ~f:(fun (env, prev) expr ->
            let env, expr = optimize_expr env expr in
            (* This (correctly) reverses the args *)
            (env, expr :: prev))
      in
      (env, FuncInvoc (receiver, args, pos))
  | IdRef (name, pos) -> (
      let name_opt = Environment.find env name in
      match name_opt with
      | None ->
          let msg = Printf.sprintf "Undeclared identifier %s" name in
          fail ~env ~pos msg
      | Some _ -> (env, IdRef (name, pos)))
  | ContextId (name, pos) -> (
      let name_opt = Environment.find_ctx env name in
      match name_opt with
      | None ->
          let msg = Printf.sprintf "Undeclared identifier %s" name in
          fail ~env ~pos msg
      | Some _ -> (env, ContextId (name, pos)))
  | ProtoRef (name, pos) -> (
      (* Validate this class name is defined by our STDLIB *)
      match Environment.find env name with
      | Some _ -> (env, IdRef (name, pos))
      | None -> fail ~env ~pos @@ Printf.sprintf "Undeclared class %s" name)
  | FuncExpr { parameters; block; pos } ->
      let parameters2 = List.rev parameters in
      let env2 = Environment.push_empty env in
      let env3 =
        List.fold_left parameters2 ~init:env2 ~f:(fun env param ->
            let param, pos = param in
            match Environment.bind env param with
            | None ->
                Printf.sprintf "Duplicate parameter named %s" param
                |> fail ~env ~pos
            | Some e -> e)
      in
      let block2 = optimize_block env3 block in
      (* We return the same input env *)
      (env, FuncExpr { parameters = parameters2; block = block2; pos })
  | IfExpr (cond_cont, pos) ->
      let env, cont = optimize_continuation env cond_cont in
      (env, IfExpr (cont, pos))
  | UnaryExpr { target; operator; pos } ->
      let env, target = optimize_expr env target in
      (env, UnaryExpr { target; operator; pos })
  | MethodInvoc { target; receiver; args; pos } ->
      let env, optim_receiver = optimize_expr env receiver in
      let env, args =
        List.fold args ~init:(env, []) ~f:(fun (env, prev) arg ->
            let env, arg = optimize_expr env arg in
            (* This (correctly) reverses the args *)
            (env, arg :: prev))
      in
      (env, MethodInvoc { target; receiver = optim_receiver; args; pos })
  | DoBlock (block, pos) -> (env, DoBlock (optimize_block env block, pos))
  | ObjDeref (receiver, name, pos) ->
      let env, receiver = optimize_expr env receiver in
      (env, ObjDeref (receiver, name, pos))
  | LetExpr (name, expr, pos) ->
      (* TODO verify *)
      let env, e = optimize_expr env expr in
      let env =
        match Environment.bind env name with
        | None ->
            Printf.sprintf "The name %s has already been declared in this scope"
              name
            |> fail ~env ~pos
        | Some e -> e
      in
      (env, LetExpr (name, e, pos))
  | AssignExpr (name, expr, pos) ->
      (* TODO verify *)
      let env, e = optimize_expr env expr in
      (env, AssignExpr (name, e, pos))
  | SubAssignExpr { subscript; value; pos } ->
      let env, subscript = optimize_expr env subscript in
      let env, value = optimize_expr env value in
      (env, SubAssignExpr { subscript; value; pos })
  | ForLoop (init, comp, inc, block, pos) ->
      let inner_env = Environment.push_empty env in
      let inner_env, init' = optimize_expr inner_env init in
      let inner_env, comp' = optimize_expr inner_env comp in
      let inner_env, inc' = optimize_expr inner_env inc in
      let block' = optimize_block inner_env block in
      (env, ForLoop (init', comp', inc', block', pos))
  | ForInLoop { iterator_name; iteratee; block; pos } ->
      let inner_env = Environment.push_empty env in
      let inner_env =
        Environment.bind inner_env iterator_name |> Option.value_exn
      in
      let inner_env, iteratee = optimize_expr inner_env iteratee in
      let block = optimize_block inner_env block in
      (env, ForInLoop { iterator_name; iteratee; block; pos })
  | WithExpr (assignments, block, pos) ->
      let env, assignments =
        List.fold assignments ~init:(env, []) ~f:(fun (env, prev) assignment ->
            let name, expr = assignment in
            let env, expr = optimize_expr env expr in
            (env, (name, expr) :: prev))
      in
      (* We don't need to push empty context frames *)
      let inner_env = Environment.push_empty env in
      let block = optimize_block inner_env block in
      (env, WithExpr (assignments, block, pos))

and optimize_string env s pos =
  let env, contents =
    match s with
    | FullString (s', pos) -> (env, [ FullString (s', pos) ])
    | StartStringInterp (s', cont1, pos) ->
        let env, cont2 = optimize_string_continuation env cont1 in
        (env, StartStringInterp (s', pos) :: cont2)
  in
  (env, String (contents, pos))

and optimize_string_continuation env cont =
  match cont with
  | MiddleStringInterp (e, s, cont2, pos) ->
      let env, e2 = optimize_expr env e in
      let env, cont3 = optimize_string_continuation env cont2 in
      (env, ExpressionStringInterp e2 :: MiddleStringInterp (s, pos) :: cont3)
  | EndStringInterp (e, s, pos) ->
      let env, e = optimize_expr env e in
      (env, [ ExpressionStringInterp e; EndStringInterp (s, pos) ])

and optimize_continuation env c =
  match c with
  | Ast.IfCont { conditional; block; continuation; pos } ->
      (* If this binds a new name, it escapes the scope, is this right? *)
      let env, conditional = optimize_expr env conditional in
      let env2 = Environment.push_empty env in
      let block = optimize_block env2 block in
      let env, continuation =
        match continuation with
        | None -> (env, None)
        | Some cont ->
            let env, cont = optimize_continuation env cont in
            (env, Some cont)
      in
      (env, IfCont { conditional; block; continuation; pos })
  | Ast.ElseCont (stmts, pos) ->
      let env2 = Environment.push_empty env in
      let optimized_stmts = optimize_block env2 stmts in
      (env, ElseCont (optimized_stmts, pos))

let prog_to_str stmts = sexp_of_prog stmts |> Sexp.to_string
