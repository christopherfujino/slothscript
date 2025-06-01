open Core

let rec interpret_stmt (ctx : Context.t) stmt =
  let open Compiler.Optimizer in
  match stmt with
  | LetStmt (id, e) ->
      let v = interpret_expr ctx e in
      Identifiers.set ctx.identifiers id v;
      v
  | ExprStmt expr -> interpret_expr ctx expr
  | FuncStmt { name; parameters; block } ->
      (* TODO ensure this is at top-level *)
      Functions.set ctx.functions name { parameters; block };
      Runtime.Null

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
  | FuncInvoc (name, args) ->
      let f = Functions.get ctx.functions name in
      (* TODO this is wrong if we're already in a function *)
      let identifiers = Identifiers.push_new_frame ctx.identifiers in
      let temp_ctx = { ctx with identifiers } in
      (match
         List.iter2 f.parameters args ~f:(fun p a ->
             let v = interpret_expr temp_ctx a in
             Identifiers.set temp_ctx.identifiers p v)
       with
      | Ok () -> ()
      | Unequal_lengths ->
          Printf.sprintf "Mismatched number of params and args in call to %s"
            name
          |> failwith);

      let rec traverse stmts =
        match stmts with
        | [] -> Runtime.Null
        | stmt :: stmts ->
            let return_val = interpret_stmt temp_ctx stmt in
            if List.is_empty stmts then return_val
            else (traverse [@tailrec]) stmts
      in
      traverse f.block

and interpret_prog ctx prog =
  (* TODO we need to reverse program! *)
  match prog with
  | [] -> ()
  | hd :: tl ->
      let _ = interpret_stmt ctx hd in
      interpret_prog ctx tl
