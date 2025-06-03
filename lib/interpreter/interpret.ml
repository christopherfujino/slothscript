open Core

let rec interpret_stmt (ctx : Context.t) stmt =
  let open Compiler.Optimizer in
  match stmt with
  | LetStmt (id, e) -> (
      let v = interpret_expr ctx e in
      match ctx.identifiers with
      | [] -> failwith "unreachable"
      | current_env_frame :: tail_env_frames ->
          let identifiers = Identifiers.set current_env_frame id v in
          ({ ctx with identifiers = identifiers :: tail_env_frames }, v))
  | ExprStmt expr -> (ctx, interpret_expr ctx expr)
  | FuncStmt { name; parameters; block } -> (
      match ctx.identifiers with
      | [] -> failwith "unreachable"
      | current_env_frame :: tail_env_frames ->
          let identifiers =
            Identifiers.set current_env_frame name
              (Func
                 (User
                    {
                      parameters;
                      block;
                      (* TODO this should snapshot *)
                      identifiers = ctx.identifiers;
                    }))
          in
          ( { ctx with identifiers = identifiers :: tail_env_frames },
            Runtime.Null ))

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
  | FuncInvoc (name, args) -> (
      match Identifiers.get ctx.identifiers name with
      | Func f -> (
          match f with
          | User { parameters; block; identifiers } ->
              let new_frame = Identifiers.create () in
              let new_frame =
                match
                  List.fold2 parameters args ~init:new_frame
                    ~f:(fun frame p a ->
                      let v = interpret_expr ctx a in
                      Identifiers.set frame p v)
                with
                | Ok frame -> frame
                | Unequal_lengths ->
                    Printf.sprintf
                      "Mismatched number of params and args in call to %s" name
                    |> failwith
              in

              let temp_ctx =
                { ctx with identifiers = new_frame :: identifiers }
              in
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
          Printf.sprintf "Tried to invoke id \"%s\", but it is not a function"
            name
          |> failwith)

and interpret_prog ctx prog =
  match prog with
  | [] -> (ctx, Runtime.Null)
  | hd :: tl ->
      let new_ctx, v = interpret_stmt ctx hd in
      if List.is_empty tl then (new_ctx, v)
      else (interpret_prog [@tailrec]) new_ctx tl
