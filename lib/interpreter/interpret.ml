open Core

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
  | FuncInvoc (name, exprs) ->
      let vals = List.map exprs ~f:(interpret_expr ctx) in
      let f = Functions.get ctx.functions name in
      Identifiers.push ctx.identifiers;
      (* TODO must actually manually traverse this list and return last val *)
      let unit_or_error = List.iter2 f.parameters vals ~f:(fun param arg_val ->
          Identifiers.set ctx.identifiers param arg_val) in
      match unit_or_error with
      | Ok u -> u
      | Unequal_lengths -> Printf.sprintf "mismatch of params and args in call to %s" name |> failwith

and interpret_prog ctx prog =
  (* TODO we need to reverse program! *)
  match prog with
  | [] -> ()
  | hd :: tl ->
      interpret_stmt ctx hd;
      interpret_prog ctx tl
