open Core
open Common

let ( >>= ) =
 fun (ctx, either) cb ->
  Either.value_map either
    ~second:(fun tuple -> (ctx, Second tuple))
    ~first:(cb ctx)

let failure ~ctx pos msg =
  let pos_s = Sloth_common.Position.string_of_t pos in
  let msg1 =
    Printf.sprintf "%s\n\n[%s] Runtime error: %s"
      (Sloth_common.Position.summarize pos Globals.(ctx.src))
      pos_s msg
  in
  let msg2 =
    if Sloth_common.Config.print_internal_backtraces then
      let callstack_depth = 50 in
      Printf.sprintf "%s\n\n%s" msg1
        (* Core.Printexc does not implement .get_callstack *)
        (Stdlib.Printexc.get_callstack callstack_depth
        |> Stdlib.Printexc.raw_backtrace_to_string)
    else msg1
  in
  raise (Failure msg2)

let rec interpret_prog ctx prog =
  match prog with
  | [] -> (ctx, Runtime.Null)
  | hd :: tl -> (
      let new_ctx, v = interpret_decl ctx hd in
      match tl with
      | [] -> (ctx, v)
      | _ -> (interpret_prog [@tailcall]) new_ctx tl)

and interpret_decl (ctx : Globals.t) decl =
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
      (match Identifiers.bind ctx.identifiers name f with
      | Some () -> ()
      | None ->
          Printf.sprintf "A function named %s has already been declared" name
          |> failure ~ctx pos);
      (ctx, f)
  | StmtDecl s ->
      let ctx, either = interpret_stmt ctx s in
      let v =
        match either with
        | First v -> v
        | Second (bt, _) -> (
            match bt with
            | Break | Continue | Return ->
                (* TODO: This should be caught by optimizer *)
                Sloth_common.Common.internal_failure __LOC__)
      in
      (ctx, v)

and interpret_stmt (ctx : Globals.t) stmt :
    Globals.t * (Runtime.t, Compiler.Ast.breaking_type * Runtime.t) Either.t =
  (* Globals.t * Compiler.Ast.breaking_type option * Runtime.t = *)
  let open Compiler.Optimizer in
  let ( >>= ) =
   fun (ctx, either)
       (cb :
         Globals.t ->
         Runtime.t ->
         Globals.t
         * (Runtime.t, Compiler.Ast.breaking_type * Runtime.t) Either.t) ->
    Either.value_map either
      ~second:(fun tuple -> (ctx, Second tuple))
      ~first:(cb ctx)
  in

  match stmt with
  | ExprStmt expr -> interpret_expr ctx expr
  | BreakingStmt (break_type, expr_opt, _) ->
      (match expr_opt with
      | None -> (ctx, Second (break_type, Runtime.Null))
      | Some e -> interpret_expr ctx e)
      >>= fun ctx v -> (ctx, Second (break_type, v))

and interpret_cond ctx cond =
  let ( >>= ) =
   fun (ctx, either) cb ->
    Either.value_map either
      ~second:(fun tuple -> (ctx, Second tuple))
      ~first:(cb ctx)
  in
  match cond with
  | Compiler.Optimizer.IfCont { conditional; block; continuation; pos } -> (
      interpret_expr ctx conditional >>= fun ctx condition ->
      match Runtime.bool_of_val condition with
      | Some condition_b -> (
          if condition_b then
            let inner_ctx =
              { ctx with identifiers = Identifiers.push_empty ctx.identifiers }
            in
            (ctx, interpret_block inner_ctx block)
          else
            match continuation with
            | None -> (ctx, First Runtime.Null (* TODO: Is this right? *))
            | Some cond -> (interpret_cond [@tailcall]) ctx cond)
      | None ->
          Printf.sprintf
            "If-expressions must have a boolean expression, but you used %s"
            (Runtime.to_s condition)
          |> failure ~ctx pos)
  | Compiler.Optimizer.ElseCont (stmts, _) ->
      let inner_ctx =
        { ctx with identifiers = Identifiers.push_empty ctx.identifiers }
      in
      (ctx, interpret_block inner_ctx stmts)

and interpret_expr ctx expr :
    Globals.t * (Runtime.t, Compiler.Ast.breaking_type * Runtime.t) Either.t =
  let open Compiler.Optimizer in
  match expr with
  | Num (f, _) -> (ctx, First (Runtime.Num f))
  | String (parts, _) ->
      let buf = Buffer.create 128 in
      List.fold parts ~init:(ctx, First ()) ~f:(fun (ctx, either) part ->
          (ctx, either) >>= fun ctx _ ->
          match part with
          | FullString (contents, _) ->
              (ctx, First (Buffer.add_string buf contents))
          | StartStringInterp (contents, _) ->
              (ctx, First (Buffer.add_string buf contents))
          | MiddleStringInterp (contents, _) ->
              (ctx, First (Buffer.add_string buf contents))
          | EndStringInterp (contents, _) ->
              (ctx, First (Buffer.add_string buf contents))
          | ExpressionStringInterp e ->
              interpret_expr ctx e >>= fun ctx v ->
              let s = Runtime.to_s v in
              Buffer.add_string buf s;
              (ctx, First ()))
      >>= fun ctx () -> (ctx, First (Runtime.String (Buffer.contents buf)))
  | Bool (b, _) -> (ctx, First (Runtime.Bool b))
  | Null _ -> (ctx, First Runtime.Null)
  | List (els, _) ->
      List.fold els ~init:(ctx, First []) ~f:(fun ctx_either cur ->
          ctx_either >>= fun ctx prev ->
          interpret_expr ctx cur >>= fun ctx el ->
          ( ctx,
            First
              ((* This reverses the order *)
               el :: prev) ))
      >>= fun ctx reversed_elements ->
      let els = List.rev reversed_elements in
      let arr = Array.of_list els in
      (ctx, First (Runtime.List arr))
  | HashMap (kvps, _) ->
      List.fold kvps ~init:(ctx, First []) ~f:(fun acc (k, v) ->
          acc >>= fun ctx prev ->
          interpret_expr ctx k >>= fun ctx k ->
          interpret_expr ctx v >>= fun ctx v ->
          let cur = (k, v) in
          (* Order doesn't matter here *)
          (* TODO: we could already populate the Hashtbl here *)
          (ctx, First (cur :: prev)))
      >>= fun ctx kvps' ->
      let tbl = Stdlib.Hashtbl.create 8 in
      List.iter kvps' ~f:(fun (k, v) -> Stdlib.Hashtbl.add tbl k v);
      (ctx, First (Runtime.HashMap tbl))
  | Subscript (receiver, subscript, pos) -> (
      interpret_expr ctx receiver >>= fun ctx receiver' ->
      interpret_expr ctx subscript >>= fun ctx subscript' ->
      match receiver' with
      | Runtime.List elements -> (
          match subscript' with
          | Runtime.Num idx ->
              if Float.is_integer idx then
                let i = Stdlib.int_of_float idx in
                (ctx, First (Array.get elements i))
              else
                Printf.sprintf
                  "Lists can only be subscripted by integers, you used %s"
                  (Runtime.to_s subscript')
                |> failure ~ctx pos
          | _ ->
              failure ~ctx pos
                (Runtime.to_s subscript'
                |> Printf.sprintf
                     "Lists can only be subscripted by Numbers, you used %s"))
      | Runtime.HashMap tbl -> (ctx, First (Stdlib.Hashtbl.find tbl subscript'))
      | _ ->
          Printf.sprintf "Cannot subscript the value %s"
            (Runtime.to_s receiver')
          |> failure ~ctx pos)
  | ContextId (i, pos) -> (
      match Context.get ctx.context_ids i with
      | Some v -> (ctx, First v)
      | None ->
          Printf.sprintf "The name %s has not been declared in this scope" i
          |> failure ~ctx pos)
  | IdRef (i, pos) -> (
      match Identifiers.get ctx.identifiers i with
      | Some v -> (ctx, First v)
      | None ->
          Printf.sprintf "The name %s has not been declared in this scope" i
          |> failure ~ctx pos)
  | Equality (lhs, rhs, is_equality, _) ->
      interpret_expr ctx lhs >>= fun ctx lhs ->
      interpret_expr ctx rhs >>= fun ctx rhs ->
      (ctx, First (Runtime.Bool (is_equal ctx is_equality lhs rhs)))
  | Binary (lhs, rhs, op, pos) -> interpret_binary ctx lhs rhs op pos
  | MethodInvoc { receiver; target; args; pos } ->
      interpret_method ~ctx ~pos receiver args target
  | FuncInvoc (receiver, args, pos) -> (
      interpret_expr ctx receiver >>= fun ctx -> function
      | Func f -> (
          match f with
          | User { parameters; block; identifiers } ->
              let identifiers2 = Identifiers.push_empty identifiers in
              (* Bind args to env *)
              let or_unequal =
                List.fold2 parameters args ~init:(ctx, First ())
                  ~f:(fun acc p a ->
                    acc >>= fun ctx () ->
                    interpret_expr ctx a >>= fun ctx arg_val ->
                    (* This must not throw *)
                    Identifiers.bind identifiers2 p arg_val |> Option.value_exn;
                    (ctx, First ()))
              in
              (match or_unequal with
              | Ok tuple -> tuple
              | Unequal_lengths ->
                  Printf.sprintf
                    "You passed %d arguments to a function that expected %d"
                    (List.length args) (List.length parameters)
                  |> failure ~ctx pos)
              >>= fun ctx () ->
              let temp_ctx = { ctx with identifiers = identifiers2 } in
              let rec traverse_stmts ctx stmts =
                match stmts with
                | [] -> (ctx, First Runtime.Null)
                | hd :: tl -> (
                    let ctx, either = interpret_stmt ctx hd in
                    match either with
                    | First return_val ->
                        if List.is_empty tl then (ctx, First return_val)
                        else (traverse_stmts [@tailrec]) ctx tl
                    | Second (bt, return_val) as either -> (
                        match bt with
                        | Return -> (ctx, First return_val)
                        | _ -> (ctx, either)))
              in
              (* discard context *)
              (* Note, Return has already been unwrapped *)
              let _, either = traverse_stmts temp_ctx block in
              (ctx, either)
          | Native { cb; parameters = _; identifiers = _ } -> (
              List.fold args ~init:(ctx, First []) ~f:(fun acc arg ->
                  acc >>= fun ctx prev ->
                  interpret_expr ctx arg >>= fun ctx arg ->
                  (* This is reversed... *)
                  (ctx, First (arg :: prev)))
              >>= fun ctx reversed_args ->
              let args = List.rev reversed_args in

              match cb args with
              | Ok v -> (ctx, First v)
              | Error msg ->
                  (* TODO: should this return a `Some SlothError`? *)
                  failure ~ctx pos msg))
      | Method (receiver, func_t) -> (
          match func_t with
          | Native { cb; parameters = _; identifiers = _ } -> (
              List.fold args ~init:(ctx, First []) ~f:(fun acc arg ->
                  acc >>= fun ctx prev ->
                  interpret_expr ctx arg >>= fun ctx arg ->
                  (* This is reversed... *)
                  (ctx, First (arg :: prev)))
              >>= fun ctx reversed_args ->
              let args = List.rev reversed_args in

              match cb (receiver :: args) with
              | Ok v -> (ctx, First v)
              | Error msg ->
                  (* TODO: should this return a `Some SlothError`? *)
                  failure ~ctx pos msg)
          | User _ ->
              (* I think this is unreachable... *)
              Sloth_common.Common.internal_failure __LOC__)
      | Prototype { name } -> (
          if not @@ phys_equal (List.length args) 1 then failwith "TODO"
          else
            let arg_expr = List.hd_exn args in
            interpret_expr ctx arg_expr >>= fun ctx arg ->
            match name with
            | "File" -> (ctx, First (Runtime.File (cast_to_file ~ctx ~pos arg)))
            | _ -> Sloth_common.Common.internal_failure __LOC__)
      | _ as t ->
          Printf.sprintf "Tried to invoke %s, but it is not a function"
            (Runtime.to_s t)
          |> failwith)
  | FuncExpr { parameters; block; _ } ->
      let parameters = List.map parameters ~f:(fun (name, _) -> name) in
      let u =
        Runtime.User { parameters; block; identifiers = ctx.identifiers }
      in
      let f = Runtime.Func u in
      (ctx, First f)
  | IfExpr (cond, _) -> interpret_cond ctx cond
  | UnaryExpr { target; pos; operator } -> (
      interpret_expr ctx target >>= fun ctx v ->
      match operator with
      | Not -> (
          let bool_opt = Runtime.bool_of_val v in
          match bool_opt with
          | None ->
              Runtime.to_s v
              |> Printf.sprintf
                   "The `not` operator must be applied to a Bool value, but \
                    got %s"
              |> failure ~ctx pos
          | Some b -> (ctx, First (Runtime.Bool (not b))))
      | Bang -> (
          interpret_expr ctx target >>= fun ctx target ->
          let proc = cast_to_process ~ctx ~pos target in
          match Globals.exec_proc proc with
          | Ok t' -> (ctx, First t')
          | Error err -> failure ~ctx pos err)
      | LeftArrow -> (
          interpret_expr ctx target >>= fun ctx target ->
          let file = cast_to_file ~ctx ~pos target in
          let target = Runtime.File file in
          let func = dereference_object ctx target "readString" pos in
          let receiver, func =
            match Runtime.method_of_val func with
            | None -> Sloth_common.Common.internal_failure __LOC__
            | Some func -> func
          in
          let cb =
            match func with
            | User _ -> Sloth_common.Common.internal_failure __LOC__
            | Native { cb; _ } -> cb
          in
          match cb [ receiver; target ] with
          | Error err -> failure ~ctx pos err
          | Ok t' -> (ctx, First t'))
      | Plus | Minus | Product | Divide | Pipe | Less | Greater | Leq | Geq
      | RightArrow ->
          (* Unreachable *) Sloth_common.Common.internal_failure __LOC__)
  | DoBlock (block, _) ->
      let inner_ctx =
        { ctx with identifiers = Identifiers.push_empty ctx.identifiers }
      in
      (ctx, interpret_block inner_ctx block)
  | ObjDeref (receiver, target, pos) ->
      interpret_expr ctx receiver >>= fun ctx receiver ->
      (ctx, First (dereference_object ctx receiver target pos))
  | LetExpr (id, e, pos) ->
      interpret_expr ctx e >>= fun ctx v ->
      (match Identifiers.bind ctx.identifiers id v with
      | Some () -> ()
      | None ->
          Printf.sprintf
            "The name %s has already been declared in this scope; did you mean \
             to assign to it?"
            id
          |> failure ~ctx pos);
      (ctx, First v)
  | AssignExpr (id, e, pos) -> (
      interpret_expr ctx e >>= fun ctx v ->
      match Identifiers.reassign ctx.identifiers id v with
      | Some () -> (ctx, First v)
      | None ->
          Printf.sprintf
            "The name %s has not been declared yet; did you mean to declare it?"
            id
          |> failure ~ctx pos)
  | SubAssignExpr { subscript; value; pos = _ } -> (
      match subscript with
      | Subscript (receiver, subscript, pos) -> (
          interpret_expr ctx receiver >>= fun ctx receiver' ->
          interpret_expr ctx subscript >>= fun ctx subscript' ->
          interpret_expr ctx value >>= fun ctx value' ->
          match receiver' with
          | HashMap tbl ->
              Stdlib.Hashtbl.replace tbl subscript' value';
              (ctx, First value')
          | List elements -> (
              match Runtime.int_of_val subscript' with
              | Some i ->
                  Array.set elements i value';
                  (ctx, First receiver')
              | None ->
                  Printf.sprintf
                    "Lists can only be subscripted with Numbers, but you used \
                     %s"
                    (Runtime.to_s subscript')
                  |> failure ~ctx pos)
          | _ ->
              Printf.sprintf "Assigning via subscript to %s not implemented"
                (Runtime.to_s receiver')
              |> failure ~ctx pos)
      | _ -> Sloth_common.Common.internal_failure __LOC__)
  | ForLoop (init, cmp, inc, bl, pos) ->
      let _, either =
        let identifiers = Identifiers.push_empty ctx.identifiers in
        let ctx' = { ctx with identifiers } in
        let ctx'', either = interpret_expr ctx' init in
        (match either with
        | Second (bt, _) -> (
            match bt with
            | Continue | Break | Return ->
                (* TODO optimizer should check for this *)
                Sloth_common.Common.internal_failure __LOC__)
        | First _ -> (ctx'', Either.First Runtime.Null))
        >>= fun ctx _ ->
        let rec interpret_for_loop ctx cmp inc bl (last_val : Runtime.t) =
          let ctx, bt_either = interpret_expr ctx cmp in
          ( ctx,
            match bt_either with
            | First _ -> bt_either
            | Second (bt, _) -> (
                match bt with
                | Return ->
                    (* This is reachable if the expression was a do block with
                       a return statement in it *)
                    bt_either
                | _ ->
                    Printf.sprintf
                      "TODO: figure out how to handle break/continue within \
                       for loop comparison %s"
                      __LOC__
                    |> failwith) )
          >>= fun ctx cmp_val ->
          match Runtime.bool_of_val cmp_val with
          | Some cmp_val -> (
              if not cmp_val then (ctx, First last_val)
              else
                (* Each iteration should have its own scope *)
                let inner_ctx =
                  Globals.
                    {
                      ctx with
                      identifiers = Identifiers.push_empty ctx.identifiers;
                    }
                in

                let recurse ret_val =
                  (* this iteration had no breaking stmt *)
                  let ctx, either = interpret_expr ctx inc in
                  match either with
                  | First _ ->
                      (interpret_for_loop [@tailcall]) ctx cmp inc bl ret_val
                  | Second (bt, _) -> (
                      match bt with
                      | Return ->
                          (* This is reachable if the expression was a do block *)
                          (ctx, either)
                      | _ ->
                          Printf.sprintf
                            "TODO: figure out how to handle break/continue \
                             within for loop increment %s"
                            __LOC__
                          |> failwith)
                in

                match interpret_block inner_ctx bl with
                | First v ->
                    (* The inner block cannot bind new names accessible out here *)
                    recurse v
                | Second (bt, break_val) as either -> (
                    match bt with
                    | Return ->
                        (* let returns bubble up *)
                        (ctx, either)
                    | Break ->
                        (* Done with loop, return break_val *)
                        (ctx, First break_val)
                    | Continue -> recurse break_val))
          | None ->
              Printf.sprintf
                "The comparison of a for loop must be a Boolean value, but you \
                 used %s"
                (Runtime.to_s cmp_val)
              |> failure ~ctx pos
        in
        let _, either = interpret_for_loop ctx cmp inc bl Runtime.Null in
        (ctx, either)
      in
      (ctx, either)
      (* for <iterator_name> in <iteratee> { <block> } *)
  | ForInLoop { iterator_name; iteratee; block; pos } ->
      let ctx =
        { ctx with identifiers = Identifiers.push_empty ctx.identifiers }
      in
      interpret_expr ctx iteratee >>= fun ctx iteratee ->
      let iteratee_array =
        match iteratee with
        | List l -> l
        | _ ->
            Printf.sprintf "Cannot iterate over a %s"
              (Runtime.to_class_name iteratee)
            |> failure ~ctx pos
      in
      ( ctx,
        Array.fold iteratee_array ~init:(First Runtime.Null)
          ~f:(fun prev element ->
            if Either.is_second prev then prev
            else
              let temp_ctx =
                {
                  ctx with
                  identifiers = Identifiers.push_empty ctx.identifiers;
                }
              in
              Identifiers.bind temp_ctx.identifiers iterator_name element
              |> Option.value_exn;
              interpret_block temp_ctx block) )
      >>= fun _ ret_val -> (ctx, First ret_val)
  | WithExpr (assignments, block, pos) ->
      let globals = ctx in
      let post_block_hook = ref None in
      let inner_globals =
        { globals with context_ids = Context.push_empty globals.context_ids }
      in
      let _, either =
        List.fold assignments ~init:(inner_globals, First ())
          ~f:(fun prev (name, expr) ->
            prev >>= fun globals () ->
            interpret_expr globals expr >>= fun globals v ->
            (match Context.reassign globals.context_ids name v with
            | Some () -> ()
            | None ->
                Printf.sprintf "The context variable named %s is not defined"
                  name
                |> failure ~ctx:globals pos);
            (* Process side effects for certain context variables *)
            (match name with
            | "$cwd" ->
                (* TODO: catch type error *)
                let old_cwd = Sys_unix.getcwd () in
                Runtime.string_of_val v |> Option.value_exn |> Core_unix.chdir;
                post_block_hook := Some (fun () -> Core_unix.chdir old_cwd)
            | _ -> ());
            (globals, First ()))
        >>= fun globals () -> (globals, interpret_block globals block)
      in
      (* This needs to run regardless of either's state *)
      let _ = Option.map !post_block_hook ~f:(fun hook -> hook ()) in
      (globals, either)

(** You must push an empty env frame on first *)
and interpret_block (ctx : Globals.t) (stmts : Compiler.Optimizer.stmt list) :
    (Runtime.t, Compiler.Ast.breaking_type * Runtime.t) Either.t =
  (* TODO can't use List.fold_left because we want to handle empty list
     differently *)
  let rec traverse_stmts ctx' stmts =
    match stmts with
    | [] -> First Runtime.Null
    | hd :: tl -> (
        let ctx'', either = interpret_stmt ctx' hd in
        match either with
        | Second (bt, v) -> Second (bt, v)
        | First v ->
            if List.is_empty tl then First v
            else (traverse_stmts [@tailcall]) ctx'' tl)
  in
  traverse_stmts ctx stmts

and interpret_method ~ctx ~pos receiver args method_name =
  let ( >>= ) =
   fun (ctx, either) cb ->
    Either.value_map either
      ~second:(fun tuple -> (ctx, Second tuple))
      ~first:(cb ctx)
  in
  interpret_expr ctx receiver >>= fun ctx receiver ->
  List.fold args ~init:(ctx, First []) ~f:(fun acc cur ->
      acc >>= fun ctx prev ->
      interpret_expr ctx cur >>= fun ctx arg ->
      (* This is reversed... *)
      (ctx, First (arg :: prev)))
  >>= fun ctx reversed_args ->
  let args = List.rev reversed_args in
  let class_name = Runtime.to_class_name receiver in
  let klass =
    match Hashtbl.find ctx.classes class_name with
    | None -> Sloth_common.Common.internal_failure __LOC__
    | Some klass -> klass
  in
  match Hashtbl.find klass.instance_members method_name with
  | None ->
      Printf.sprintf "The class %s does not have an instance field named %s"
        class_name method_name
      |> failure ~ctx pos
  | Some func -> (
      match func with
      | Func func -> (
          match func with
          | User _ -> Sloth_common.Common.internal_failure __LOC__
          | Native { cb; _ } -> (
              let args = receiver :: args in
              match cb args with
              | Ok v -> (ctx, First v)
              | Error msg ->
                  (* TODO propagate SlothError *) failure ~ctx pos msg))
      | _ ->
          Printf.sprintf "Internal error: %s\n\n%s"
            (Runtime.to_class_name func)
            __LOC__
          |> failwith)

and is_equal ctx is_equality lhs rhs =
  let lh_s = Runtime.to_class_name lhs in
  let rh_s = Runtime.to_class_name rhs in
  let same_class = String.equal lh_s rh_s in
  (* if ==, then return false; if !=, then return true *)
  if not same_class then not is_equality
  else if not @@ String.equal lh_s rh_s then false
  else
    match lhs with
    | String lh_s -> (
        let rh_s = Runtime.string_of_val rhs |> Option.value_exn in
        match is_equality with
        | true -> String.equal lh_s rh_s
        | false -> not @@ String.equal lh_s rh_s)
    | Num lhs -> (
        let rhs = Runtime.num_of_val rhs |> Option.value_exn in
        match is_equality with
        | true -> Float.equal lhs rhs
        | false -> not @@ Float.equal lhs rhs)
    | Bool lhs -> (
        let rhs = Runtime.bool_of_val rhs |> Option.value_exn in
        match is_equality with
        | true -> Bool.(lhs = rhs)
        | false -> Bool.((not lhs) = rhs))
    | List lhs ->
        let rhs = Runtime.list_of_val rhs |> Option.value_exn in
        let left_len = Array.length lhs in
        let right_len = Array.length rhs in
        if (not is_equality) && not (phys_equal left_len right_len) then true
        else
          let is_deep_equal = Array.equal (is_equal ctx true) lhs rhs in
          Bool.(is_deep_equal = is_equality)
    | HashMap lhs ->
        let rhs = Runtime.hashmap_of_val rhs |> Option.value_exn in
        let left_len = Stdlib.Hashtbl.length lhs in
        let right_len = Stdlib.Hashtbl.length rhs in
        if (not is_equality) && not (phys_equal left_len right_len) then true
        else
          let is_deep_equal =
            Stdlib.Hashtbl.fold
              (fun key left_value equal_so_far ->
                if not equal_so_far then false
                else
                  match Stdlib.Hashtbl.find_opt rhs key with
                  | None -> false
                  | Some right_value -> is_equal ctx true left_value right_value)
              lhs true
          in
          Bool.(is_deep_equal = is_equality)
    | Null -> ( match rhs with Null -> is_equality | _ -> not is_equality)
    | _ ->
        Printf.sprintf "is_equal the type %s is not implemented" lh_s
        |> failwith

and dereference_object ctx receiver target pos =
  let descriptor, class_name, table_thunk =
    let open Runtime in
    match receiver with
    (* Static access has different semantics *)
    | Prototype { name } -> ("static", name, fun cl -> cl.static_members)
    | _ ->
        ( "instance",
          Runtime.to_class_name receiver,
          fun cl -> cl.instance_members )
  in
  match Hashtbl.find ctx.classes class_name with
  | None ->
      Printf.sprintf
        "Internal error: could not find prototype for the %s class (%s)"
        class_name __LOC__
      |> failure ~ctx pos
  | Some klass -> (
      match Hashtbl.find (table_thunk klass) target with
      | None ->
          Printf.sprintf "The class %s does not have a %s field named %s"
            class_name descriptor target
          |> failure ~ctx pos
      | Some field -> (
          match field with
          | Func func_t -> Method (receiver, func_t)
          | _ -> failure ~ctx pos "TODO"))

and cast_to_file ~ctx ~pos = function
  | Runtime.File f -> f
  | Runtime.String filename -> (
      let intermediate =
        dereference_object ctx (Runtime.Prototype { name = "File" }) "new" pos
      in
      let func_opt = Runtime.method_of_val intermediate in
      let receiver, constructor =
        match func_opt with
        | None ->
            Printf.sprintf "[%s] While trying to retrieve File.new, got %s"
              __LOC__
              (Runtime.to_s intermediate)
            |> Sloth_common.Common.internal_failure
        | Some func -> func
      in
      let callback =
        match constructor with
        | Native { cb; _ } -> cb
        | User _ -> Sloth_common.Common.internal_failure __LOC__
      in
      match callback [ receiver; Runtime.String filename ] with
      | Ok file -> (cast_to_file [@tailcall]) ~ctx ~pos file
      | Error err -> failure ~ctx pos err)
  | _ as t' ->
      failure ~ctx pos
      @@ Printf.sprintf "Expected a File but got a %s"
      @@ Runtime.to_s t'

and cast_to_process ~ctx ~pos = function
  | Runtime.Process p -> p
  | Runtime.List _ as l ->
      let constructor =
        dereference_object ctx
          (Runtime.Prototype { name = "Process" })
          "new" pos
      in
      let callback =
        match constructor with
        | Func func_t -> (
            match func_t with
            | Native { cb; _ } -> fun () -> cb [ l ]
            | User _ -> Sloth_common.Common.internal_failure __LOC__)
        | Method (receiver, func_t) -> (
            match func_t with
            | User _ -> Sloth_common.Common.internal_failure __LOC__
            | Native { cb; _ } -> fun () -> cb [ receiver; l ])
        | _ -> Sloth_common.Common.internal_failure __LOC__
      in
      let proc =
        match callback () with
        | Error err -> failure ~ctx pos err
        | Ok proc -> proc
      in
      (cast_to_process [@tailcall]) ~ctx ~pos proc
  | Runtime.String s ->
      let list =
        shell_like_escape s
        |> List.map ~f:(fun s -> Runtime.String s)
        |> Array.of_list
      in
      (cast_to_process [@tailcall]) ~ctx ~pos (Runtime.List list)
  | _ as t' ->
      failure ~ctx pos
      @@ Printf.sprintf "Expected a Process, but got a %s"
      @@ Runtime.to_s t'

and interpret_binary ctx lhs rhs op pos =
  let ( >>= ) =
   fun (ctx, either) cb ->
    Either.value_map either ~first:(cb ctx) ~second:(fun tuple ->
        (ctx, Second tuple))
  in
  interpret_expr ctx lhs >>= fun ctx lhs ->
  interpret_expr ctx rhs >>= fun ctx rhs ->
  let cast_to_number = function
    | Runtime.Num f -> f
    | Runtime.String s -> (
        match Float.of_string_opt s with
        | Some f -> f
        | None ->
            failure ~ctx pos
            @@ Printf.sprintf "Expected a Number, but got the string \"%s\"" s)
    | _ as t' ->
        failure ~ctx pos
        @@ Printf.sprintf "Expected a Number, but got a %s"
        @@ Runtime.to_s t'
  in
  match op with
  | Pipe ->
      let left = cast_to_process ~ctx ~pos lhs in
      let right = cast_to_process ~ctx ~pos rhs in
      let read, write = Core_unix.pipe () in
      left.stdout <- write;
      left.pipes_to_collect <- write :: left.pipes_to_collect;
      right.stdin <- read;
      right.pipes_to_collect <- read :: right.pipes_to_collect;
      let right = { right with previous = Some left } in
      (ctx, Either.first @@ Runtime.Process right)
  | Plus ->
      let left = cast_to_number lhs in
      let right = cast_to_number rhs in
      (ctx, Either.first @@ Runtime.Num (left +. right))
  | Minus ->
      let left = cast_to_number lhs in
      let right = cast_to_number rhs in
      (ctx, Either.first @@ Runtime.Num (left -. right))
  | Divide ->
      let left = cast_to_number lhs in
      let right = cast_to_number rhs in
      (ctx, Either.first @@ Runtime.Num (left /. right))
  | Product ->
      let left = cast_to_number lhs in
      let right = cast_to_number rhs in
      (ctx, Either.first @@ Runtime.Num (left *. right))
  | Leq ->
      let left = cast_to_number lhs in
      let right = cast_to_number rhs in
      (ctx, Either.first @@ Runtime.Bool Float.(left <= right))
  | Geq ->
      let left = cast_to_number lhs in
      let right = cast_to_number rhs in
      (ctx, Either.first @@ Runtime.Bool Float.(left >= right))
  | Less ->
      let left = cast_to_number lhs in
      let right = cast_to_number rhs in
      (ctx, Either.first @@ Runtime.Bool Float.(left < right))
  | Greater ->
      let left = cast_to_number lhs in
      let right = cast_to_number rhs in
      (ctx, Either.first @@ Runtime.Bool Float.(left > right))
  | RightArrow ->
      let left = cast_to_string ~ctx ~pos lhs in
      let Runtime.{ path } = cast_to_file ~ctx ~pos rhs in
      Out_channel.write_all path ~data:left;
      (ctx, Either.first @@ Runtime.String left)
  | Bang | Not | LeftArrow ->
      (* Not binary ops, unreachable *)
      Sloth_common.Common.internal_failure __LOC__

and cast_to_string ~ctx ~pos t' =
  let open Runtime in
  match t' with
  | String s -> s
  | ProcessResult { stdout; _ } -> stdout
  | Process proc -> (
      match Globals.exec_proc proc with
      | Ok t' -> (cast_to_string [@tailcall]) ~ctx ~pos t'
      | Error err -> failure ~ctx pos err)
  | _ as t' ->
      failure ~ctx pos
      @@ Printf.sprintf "Expected a String, but got a %s"
      @@ Runtime.to_s t'
