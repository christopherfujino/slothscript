open Core

let check_arity desired_count actual_list name =
  let actual_count = List.length actual_list in
  if not (desired_count = actual_count) then
    raise
      (Common.Failure
         (Printf.sprintf "The function %s expected %d arguments but received %d"
            name desired_count actual_count))

let rec interpret_prog ctx prog =
  match prog with
  | [] -> (ctx, Runtime.Null)
  | hd :: tl -> (
      let new_ctx, v = interpret_decl ctx hd in
      match tl with
      | [] -> (ctx, v)
      | _ -> (interpret_prog [@tailcall]) new_ctx tl)

and interpret_decl (ctx : Context.t) decl =
  let open Compiler.Optimizer in
  match decl with
  | FuncDecl { name; parameters; block } ->
      let f =
        Runtime.Func
          (User
             {
               parameters;
               block;
               (* TODO this should snapshot *)
               identifiers = ctx.identifiers;
             })
      in
      Identifiers.bind ctx.identifiers name f;
      (ctx, f)
  | StmtDecl s -> interpret_stmt ctx s

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
  | SubAssignStmt { subscript; value } -> (
      match subscript with
      | Subscript (receiver, subscript) -> (
          let receiver' = interpret_expr ctx receiver in
          let subscript' = interpret_expr ctx subscript in
          let value' = interpret_expr ctx value in
          match receiver' with
          | HashMap tbl ->
              Stdlib.Hashtbl.add tbl subscript' value';
              (ctx, value')
          | List elements ->
              let i = Runtime.int_of_val subscript' in
              Array.set elements i value';
              (ctx, receiver')
          | _ ->
              raise
                (Common.Failure
                   (Printf.sprintf
                      "Assigning via subscript to %s not implemented"
                      (Runtime.to_s receiver'))))
      | _ -> failwith "TODO")
  | ExprStmt expr -> (ctx, interpret_expr ctx expr)
  | ForLoop (init, cmp, inc, bl) ->
      let identifiers = Identifiers.push_empty ctx.identifiers in
      let ctx' = { ctx with identifiers } in
      let ctx'', _ = interpret_stmt ctx' init in

      let rec interpret_for_loop ctx cmp inc bl last_val =
        let cmp_val = interpret_expr ctx cmp |> Runtime.bool_of_val in
        if not cmp_val then last_val
        else
          let cur_val = interpret_block ctx bl in
          let ctx, _ = interpret_stmt ctx inc in
          (interpret_for_loop [@tailcall]) ctx cmp inc bl cur_val
      in
      (ctx, interpret_for_loop ctx'' cmp inc bl Runtime.Null)

and interpret_cond ctx cond =
  match cond with
  | Compiler.Optimizer.IfCont { conditional; block; continuation } -> (
      let condition = interpret_expr ctx conditional in
      let condition_b = Runtime.bool_of_val condition in
      if condition_b then
        let ctx =
          { ctx with identifiers = Identifiers.push_empty ctx.identifiers }
        in
        interpret_block ctx block
      else
        match continuation with
        | None -> Runtime.Null
        | Some cond -> (interpret_cond [@tailcall]) ctx cond)
  | Compiler.Optimizer.ElseCont stmts ->
      let ctx =
        { ctx with identifiers = Identifiers.push_empty ctx.identifiers }
      in
      interpret_block ctx stmts

(* TODO Note this does not return a context--can expressions mutate context?! *)
(* Yes, if they call a function that mutates a global *)
and interpret_expr ctx expr =
  let open Compiler.Optimizer in
  match expr with
  | Num f -> Runtime.Num f
  | String parts ->
      let buf = Buffer.create 128 in
      List.iter parts ~f:(fun part ->
          match part with
          | FullString contents -> Buffer.add_string buf contents
          | StartStringInterp contents -> Buffer.add_string buf contents
          | MiddleStringInterp contents -> Buffer.add_string buf contents
          | EndStringInterp contents -> Buffer.add_string buf contents
          | ExpressionStringInterp e ->
              let v = interpret_expr ctx e in
              let s = Runtime.to_s v in
              Buffer.add_string buf s);
      Runtime.String (Buffer.contents buf)
  | Bool b -> Runtime.Bool b
  | Null -> Runtime.Null
  | List els ->
      let arr = List.map els ~f:(interpret_expr ctx) |> Array.of_list in
      Runtime.List arr
  | HashMap kvps ->
      let kvps' =
        List.map kvps ~f:(fun (k, v) ->
            (interpret_expr ctx k, interpret_expr ctx v))
      in
      let tbl = Stdlib.Hashtbl.create 8 in
      List.iter kvps' ~f:(fun (k, v) -> Stdlib.Hashtbl.add tbl k v);
      HashMap tbl
  | Subscript (receiver, subscript) -> (
      let receiver' = interpret_expr ctx receiver in
      let subscript' = interpret_expr ctx subscript in
      match receiver' with
      | Runtime.List elements -> (
          match subscript' with
          | Runtime.Num idx ->
              if Float.is_integer idx then
                let i = Stdlib.int_of_float idx in
                Array.get elements i
              else
                Common.Failure
                  (Printf.sprintf
                     "Lists can only be subscripted by integers, you used %s"
                     (Runtime.to_s subscript'))
                |> raise
          | _ ->
              Common.Failure
                (Runtime.to_s subscript'
                |> Printf.sprintf
                     "Lists can only be subscripted by nums, you used %s")
              |> raise)
      | Runtime.HashMap tbl -> Stdlib.Hashtbl.find tbl subscript'
      | _ ->
          raise
            (Common.Failure
               (Printf.sprintf "Cannot subscript the value %s"
                  (Runtime.to_s receiver'))))
  | Binary (op, lhs, rhs) -> (
      match op with
      | Add ->
          let left_val = Runtime.num_of_val (interpret_expr ctx lhs) in
          let right_val = Runtime.num_of_val (interpret_expr ctx rhs) in
          Runtime.Num (left_val +. right_val))
  | IdRef i -> Identifiers.get ctx.identifiers i
  | MethodInvoc { receiver; target; args } -> (
      let rt_receiver = interpret_expr ctx receiver in
      let not_implemented receiver =
        let msg =
          Printf.sprintf "The type %s does not implement the method %s" receiver
            target
        in
        raise (Common.Failure msg)
      in
      match rt_receiver with
      | Null -> raise (Common.Failure "NPE!")
      | String _ -> not_implemented "String"
      | Num f -> (
          (* Does it matter this is O(n)? *)
          match target with
          | "+" ->
              check_arity 1 args "Num.+";
              let arg = List.hd_exn args in
              let arg_val = interpret_expr ctx arg in
              let arg_f = Runtime.num_of_val arg_val in
              Num (f +. arg_f)
          | "-" ->
              check_arity 1 args "Num.-";
              let arg = List.hd_exn args in
              let arg_val = interpret_expr ctx arg in
              let arg_f = Runtime.num_of_val arg_val in
              Num (f -. arg_f)
          | "*" ->
              check_arity 1 args "Num.*";
              let arg = List.hd_exn args in
              let arg_val = interpret_expr ctx arg in
              let arg_f = Runtime.num_of_val arg_val in
              Num (f *. arg_f)
          | "/" ->
              check_arity 1 args "Num./";
              let arg = List.hd_exn args in
              let arg_val = interpret_expr ctx arg in
              let arg_f = Runtime.num_of_val arg_val in
              Num (f /. arg_f)
          | "<=" ->
              check_arity 1 args "Num.<=";
              let arg = List.hd_exn args in
              let arg_val = interpret_expr ctx arg in
              let arg_f = Runtime.num_of_val arg_val in
              Bool (Float.( <= ) f arg_f)
          | _ -> not_implemented "Num")
      | _ -> failwith "TODO")
  | FuncInvoc (receiver, args) -> (
      let receiver' = interpret_expr ctx receiver in
      match receiver' with
      | Func f -> (
          match f with
          | User { parameters; block; identifiers } ->
              let identifiers2 = Identifiers.push_empty identifiers in
              (* Bind args to env *)
              (match
                 List.iter2 parameters args ~f:(fun param_name arg_expr ->
                     (* Note this is interpreted with the enclosing env *)
                     let v = interpret_expr ctx arg_expr in
                     Identifiers.bind identifiers2 param_name v)
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
  | IfExpr cond -> interpret_cond ctx cond

(** You must push an empty env frame on first *)
and interpret_block ctx stmts =
  (* TODO can't use List.fold_left because we want to handle empty list
     differently *)
  let rec traverse_stmts ctx' stmts =
    match stmts with
    | [] -> (ctx', Runtime.Null)
    | stmt :: stmts ->
        let ctx'', return_val = interpret_stmt ctx' stmt in
        if List.is_empty stmts then (ctx'', return_val)
        else (traverse_stmts [@tailcall]) ctx'' stmts
  in
  (* discard context *)
  let _, v = traverse_stmts ctx stmts in
  v
