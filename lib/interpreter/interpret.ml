let rec interpret_stmt (ctx : Context.t) stmt =
  let open Compiler.Ast in
  match stmt with
  | LetStmt (id, e) ->
      let v = interpret_expr ctx e in
      Identifiers.set ctx.identifiers id v
  | ExprStmt expr ->
      let v = interpret_expr ctx expr in
      let module L = (val ctx.l) in
      L.InputOutput.print v
  | FuncStmt { name; parameters; block } ->
      Functions.set ctx.functions name { parameters; block }

and interpret_expr ctx (expr : Compiler.Ast.expr) =
  let open Compiler.Ast in
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

and interpret_prog ctx prog =
  (* TODO we need to reverse program! *)
  match prog with
  | [] -> ()
  | hd :: tl ->
      interpret_stmt ctx hd;
      interpret_prog ctx tl
