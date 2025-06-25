open Core

let rec interpret_decl (ctx : Context.t) decl =
  let open Compiler.Optimizer in
  match decl with
  | FuncDecl { name; parameters; block } ->
      Identifiers.bind ctx.identifiers name
        (Func
           (User
              {
                parameters;
                block;
                (* TODO this should snapshot *)
                identifiers = ctx.identifiers;
              }));
      ctx
  | StmtDecl s ->
      let ctx, _ = interpret_stmt ctx s in
      ctx

and interpret_stmt (ctx : Context.t) stmt =
  let open Compiler.Optimizer in
  match stmt with
  | LetStmt (id, e) ->
      let v = interpret_expr ctx e in
      Identifiers.bind ctx.identifiers id v;
      (ctx, v)
  | AssignStmt (id, e) ->
      let v = interpret_expr ctx e in
      Identifiers.reassign ctx.identifiers id v;
      (ctx, v)
  | ExprStmt expr -> (ctx, interpret_expr ctx expr)

(* TODO Note this does not return a context--can expressions mutate context?! *)
(* Yes, if they call a function that mutates a global *)
and interpret_expr ctx expr =
  let open Compiler.Optimizer in
  match expr with
  | Num f -> Runtime.Num f
  | String s -> Runtime.String s
  | Bool b -> Runtime.Bool b
  | Binary (op, lhs, rhs) -> (
      match op with
      | Add ->
          let left_val = Runtime.num_of_val (interpret_expr ctx lhs) in
          let right_val = Runtime.num_of_val (interpret_expr ctx rhs) in
          Runtime.Num (left_val +. right_val))
  | IdRef i -> Identifiers.get ctx.identifiers i
  | FuncInvoc (receiver, args) -> (
      let receiver' = interpret_expr ctx receiver in
      match receiver' with
      | Func f -> (
          match f with
          | User { parameters; block; identifiers } ->
              let identifiers2 = Identifiers.push_empty identifiers in
              (match
                 List.iter2 parameters args ~f:(fun p a ->
                     let ctx = { ctx with identifiers } in
                     let v = interpret_expr ctx a in
                     Identifiers.bind identifiers2 p v)
               with
              | Ok () -> ()
              | Unequal_lengths ->
                  Printf.sprintf
                    "Mismatched number of params and args in call to function"
                  |> failwith);

              let temp_ctx = { ctx with identifiers = identifiers2 } in
              let rec traverse_stmts ctx stmts =
                match stmts with
                | [] -> (ctx, Runtime.Null)
                | stmt :: stmts ->
                    let ctx, return_val = interpret_stmt ctx stmt in
                    if List.is_empty stmts then (ctx, return_val)
                    else (traverse_stmts [@tailrec]) ctx stmts
              in
              (* discard context *)
              let _, v = traverse_stmts temp_ctx block in
              v
          | Native { cb; parameters = _; identifiers = _ } ->
              let vals = List.map args ~f:(interpret_expr ctx) in
              cb vals)
      | _ ->
          Printf.sprintf "Tried to invoke %s, but it is not a function"
            (Runtime.to_s receiver')
          |> failwith)
  | FuncExpr { parameters; block } ->
      Func (User { parameters; block; identifiers = ctx.identifiers })

and interpret_prog ctx prog =
  match prog with
  | [] -> ctx
  | hd :: tl ->
      let new_ctx = interpret_decl ctx hd in
      (interpret_prog [@tailcall]) new_ctx tl
