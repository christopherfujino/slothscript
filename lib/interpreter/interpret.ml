open Core
open Common

let check_arity ~pos desired_count actual_list name =
  let actual_count = List.length actual_list in
  if not (desired_count = actual_count) then
    failure pos
      (Printf.sprintf "The function %s expected %d arguments but received %d"
         name desired_count actual_count)

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
  | FuncDecl { name; parameters; block; pos } ->
      let parameters = List.map parameters ~f:(fun (name, _) -> name) in
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
      Identifiers.bind ~pos ctx.identifiers name f;
      (ctx, f)
  | StmtDecl s -> interpret_stmt ctx s

and interpret_stmt (ctx : Context.t) stmt =
  let open Compiler.Optimizer in
  match stmt with
  | LetStmt (id, e, pos) ->
      let v = interpret_expr ctx e in
      Identifiers.bind ~pos ctx.identifiers id v;
      (ctx, v)
  | AssignStmt (id, e, pos) ->
      let v = interpret_expr ctx e in
      Identifiers.reassign ~pos ctx.identifiers id v;
      (ctx, v)
  | SubAssignStmt { subscript; value; _ } -> (
      match subscript with
      | Subscript (receiver, subscript, pos) -> (
          let receiver' = interpret_expr ctx receiver in
          let subscript' = interpret_expr ctx subscript in
          let value' = interpret_expr ctx value in
          match receiver' with
          | HashMap tbl ->
              Stdlib.Hashtbl.add tbl subscript' value';
              (ctx, value')
          | List elements ->
              let i = Runtime.int_of_val pos subscript' in
              Array.set elements i value';
              (ctx, receiver')
          | _ ->
              failure pos
                (Printf.sprintf "Assigning via subscript to %s not implemented"
                   (Runtime.to_s receiver')))
      | _ -> failwith "TODO")
  | ExprStmt expr -> (ctx, interpret_expr ctx expr)
  | ForLoop (init, cmp, inc, bl, pos) ->
      let identifiers = Identifiers.push_empty ctx.identifiers in
      let ctx' = { ctx with identifiers } in
      let ctx'', _ = interpret_stmt ctx' init in

      let rec interpret_for_loop ctx cmp inc bl last_val =
        let cmp_val = interpret_expr ctx cmp |> Runtime.bool_of_val pos in
        if not cmp_val then last_val
        else
          let cur_val = interpret_block ctx bl in
          let ctx, _ = interpret_stmt ctx inc in
          (interpret_for_loop [@tailcall]) ctx cmp inc bl cur_val
      in
      (ctx, interpret_for_loop ctx'' cmp inc bl Runtime.Null)

and interpret_cond ctx cond =
  match cond with
  | Compiler.Optimizer.IfCont { conditional; block; continuation; pos } -> (
      let condition = interpret_expr ctx conditional in
      let condition_b = Runtime.bool_of_val pos condition in
      if condition_b then
        let ctx =
          { ctx with identifiers = Identifiers.push_empty ctx.identifiers }
        in
        interpret_block ctx block
      else
        match continuation with
        | None -> Runtime.Null
        | Some cond -> (interpret_cond [@tailcall]) ctx cond)
  | Compiler.Optimizer.ElseCont (stmts, _) ->
      let ctx =
        { ctx with identifiers = Identifiers.push_empty ctx.identifiers }
      in
      interpret_block ctx stmts

(* TODO Note this does not return a context--can expressions mutate context?! *)
(* Yes, if they call a function that mutates a global *)
and interpret_expr ctx expr =
  let open Compiler.Optimizer in
  match expr with
  | Num (f, _) -> Runtime.Num f
  | String (parts, _) ->
      let buf = Buffer.create 128 in
      List.iter parts ~f:(fun part ->
          match part with
          | FullString (contents, _) -> Buffer.add_string buf contents
          | StartStringInterp (contents, _) -> Buffer.add_string buf contents
          | MiddleStringInterp (contents, _) -> Buffer.add_string buf contents
          | EndStringInterp (contents, _) -> Buffer.add_string buf contents
          | ExpressionStringInterp e ->
              let v = interpret_expr ctx e in
              let s = Runtime.to_s v in
              Buffer.add_string buf s);
      Runtime.String (Buffer.contents buf)
  | Bool (b, _) -> Runtime.Bool b
  | Null _ -> Runtime.Null
  | List (els, _) ->
      let arr = List.map els ~f:(interpret_expr ctx) |> Array.of_list in
      Runtime.List arr
  | HashMap (kvps, _) ->
      let kvps' =
        List.map kvps ~f:(fun (k, v) ->
            (interpret_expr ctx k, interpret_expr ctx v))
      in
      let tbl = Stdlib.Hashtbl.create 8 in
      List.iter kvps' ~f:(fun (k, v) -> Stdlib.Hashtbl.add tbl k v);
      HashMap tbl
  | Subscript (receiver, subscript, pos) -> (
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
                failure pos
                  (Printf.sprintf
                     "Lists can only be subscripted by integers, you used %s"
                     (Runtime.to_s subscript'))
          | _ ->
              failure pos
                (Runtime.to_s subscript'
                |> Printf.sprintf
                     "Lists can only be subscripted by nums, you used %s"))
      | Runtime.HashMap tbl -> Stdlib.Hashtbl.find tbl subscript'
      | _ ->
          raise
            (Common.Failure
               (Printf.sprintf "Cannot subscript the value %s"
                  (Runtime.to_s receiver'))))
  | IdRef (i, pos) -> Identifiers.get ~pos ctx.identifiers i
  | MethodInvoc { receiver; target; args; pos } -> (
      let rt_receiver = interpret_expr ctx receiver in
      let not_implemented receiver =
        let msg =
          Printf.sprintf "The type %s does not implement the method %s" receiver
            target
        in
        failure pos msg
      in
      match rt_receiver with
      | Null -> failure pos "NPE!"
      | String _ -> not_implemented "String"
      | Num f -> (
          (* Does it matter this is O(n)? *)
          match target with
          | "+" ->
              check_arity ~pos 1 args "Num.+";
              let arg = List.hd_exn args in
              let arg_val = interpret_expr ctx arg in
              let arg_f = Runtime.num_of_val pos arg_val in
              Num (f +. arg_f)
          | "-" ->
              check_arity ~pos 1 args "Num.-";
              let arg = List.hd_exn args in
              let arg_val = interpret_expr ctx arg in
              let arg_f = Runtime.num_of_val pos arg_val in
              Num (f -. arg_f)
          | "*" ->
              check_arity ~pos 1 args "Num.*";
              let arg = List.hd_exn args in
              let arg_val = interpret_expr ctx arg in
              let arg_f = Runtime.num_of_val pos arg_val in
              Num (f *. arg_f)
          | "/" ->
              check_arity ~pos 1 args "Num./";
              let arg = List.hd_exn args in
              let arg_val = interpret_expr ctx arg in
              let arg_f = Runtime.num_of_val pos arg_val in
              Num (f /. arg_f)
          | "<=" ->
              check_arity ~pos 1 args "Num.<=";
              let arg = List.hd_exn args in
              let arg_val = interpret_expr ctx arg in
              let arg_f = Runtime.num_of_val pos arg_val in
              Bool (Float.( <= ) f arg_f)
          | _ -> not_implemented "Num")
      | _ -> failwith "TODO")
  | FuncInvoc (receiver, args, pos) -> (
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
                     (* This must not throw *)
                     Identifiers.bind ~pos:Sloth_common.Position.dummy
                       identifiers2 param_name v)
               with
              | Ok () -> ()
              | Unequal_lengths ->
                  Printf.sprintf
                    "Mismatched number of params and args in call to function"
                  |> failure pos);
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
  | FuncExpr { parameters; block; _ } ->
      let parameters = List.map parameters ~f:(fun (name, _) -> name) in
      Func (User { parameters; block; identifiers = ctx.identifiers })
  | IfExpr (cond, _) -> interpret_cond ctx cond

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
