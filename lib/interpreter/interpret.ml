open Core
open Common

let ( >>= ) =
 fun (globals, either) cb ->
  Either.value_map either
    ~second:(fun tuple -> (globals, Second tuple))
    ~first:(cb globals)

let failure_msg ~globals ~pos msg =
  let pos_s = Sloth_common.Position.string_of_t pos in
  let msg =
    Printf.sprintf "[%s] Runtime error\n\n%s\n%s" pos_s
      (Sloth_common.Position.summarize pos Globals.(globals.src))
      msg
  in
  if Sloth_common.Common.debug_mode then
    let callstack_depth = 50 in
    Printf.sprintf "%s\n\n%s" msg
      (* Core.Printexc does not implement .get_callstack *)
      (Stdlib.Printexc.get_callstack callstack_depth
      |> Stdlib.Printexc.raw_backtrace_to_string)
  else msg

let failure_obj ~globals ~pos msg =
  let msg = failure_msg ~globals ~pos msg in
  Second (Compiler.Ast.Error msg, Runtime.Null)

(**
   For error cases that could potentially become compiler errors.

   These should not be recoverable, you need to fix your code.
*)
let fail ~globals pos msg =
  let msg = failure_msg ~globals ~pos msg in
  raise (Sloth_common.Common.RuntimeError msg)

let invoke_native_func ~globals ~pos cb args =
  let either = cb args in
  match either with
  | First _ as first -> first
  | Second (bt, _) as second -> (
      match bt with
      | Compiler.Ast.Return | Break | Continue ->
          Sloth_common.Common.internal_failure __LOC__
      | Exit _ -> second
      | Error msg -> failure_obj ~globals ~pos msg)

let rec interpret_prog globals prog =
  match prog with
  | [] -> (globals, First Runtime.Null)
  | hd :: tl -> (
      interpret_decl globals hd >>= fun new_globals v ->
      match tl with
      | [] -> (globals, First v) (* TODO is this right? *)
      | _ -> (interpret_prog [@tailcall]) new_globals tl)

and interpret_decl (globals : Globals.t) decl :
    Globals.t * (Runtime.t, Compiler.Ast.breaking_type * Runtime.t) Either.t =
  let open Compiler.Optimizer in
  match decl with
  | FuncDecl { name; parameters; block; pos = _ } ->
      let parameters = List.map parameters ~f:(fun (name, _) -> name) in
      let f =
        Runtime.Func
          (User
             {
               parameters;
               block;
               (* TODO this should snapshot *)
               identifiers = globals.identifiers;
             })
      in
      (match Identifiers.bind globals.identifiers name f with
      | Some () -> ()
      | None ->
          (* This should be caught by optimizer *)
          Sloth_common.Common.internal_failure __LOC__);
      (globals, First f)
  | StmtDecl s ->
      let globals, either = interpret_stmt globals s in
      (match either with
      | First _ -> ()
      | Second (bt, _) -> (
          match bt with
          | Break | Continue | Return ->
              (* TODO: This should be caught by optimizer *)
              Sloth_common.Common.internal_failure __LOC__
          | Exit _ | Error _ -> ()));
      (globals, either)

and interpret_stmt (globals : Globals.t) stmt :
    Globals.t * (Runtime.t, Compiler.Ast.breaking_type * Runtime.t) Either.t =
  (* Globals.t * Compiler.Ast.breaking_type option * Runtime.t = *)
  let open Compiler.Optimizer in
  match stmt with
  | ExprStmt expr -> interpret_expr globals expr
  | BreakingStmt (break_type, expr_opt, _) ->
      (match expr_opt with
      | None -> (globals, Second (break_type, Runtime.Null))
      | Some e -> interpret_expr globals e)
      >>= fun globals v -> (globals, Second (break_type, v))

and interpret_cond globals cond =
  match cond with
  | Compiler.Optimizer.IfCont { conditional; block; continuation; pos } -> (
      interpret_expr globals conditional >>= fun globals condition ->
      match Runtime.bool_of_val condition with
      | Some condition_b -> (
          if condition_b then
            let inner_globals =
              {
                globals with
                identifiers = Identifiers.push_empty globals.identifiers;
              }
            in
            (globals, interpret_block inner_globals block)
          else
            match continuation with
            | None -> (globals, First Runtime.Null (* TODO: Is this right? *))
            | Some cond -> (interpret_cond [@tailcall]) globals cond)
      | None ->
          Printf.sprintf
            "If-expressions must have a boolean expression, instead received %s"
            (Runtime.to_s condition)
          |> fail ~globals pos)
  | Compiler.Optimizer.ElseCont (stmts, _) ->
      let inner_globals =
        {
          globals with
          identifiers = Identifiers.push_empty globals.identifiers;
        }
      in
      (globals, interpret_block inner_globals stmts)

and interpret_expr globals expr :
    Globals.t * (Runtime.t, Compiler.Ast.breaking_type * Runtime.t) Either.t =
  let open Compiler.Optimizer in
  match expr with
  | Num (f, _) -> (globals, First (Runtime.Num f))
  | String (parts, _) ->
      let buf = Buffer.create 128 in
      List.fold parts ~init:(globals, First ())
        ~f:(fun (globals, either) part ->
          (globals, either) >>= fun globals _ ->
          match part with
          | FullString (contents, _) ->
              (globals, First (Buffer.add_string buf contents))
          | StartStringInterp (contents, _) ->
              (globals, First (Buffer.add_string buf contents))
          | MiddleStringInterp (contents, _) ->
              (globals, First (Buffer.add_string buf contents))
          | EndStringInterp (contents, _) ->
              (globals, First (Buffer.add_string buf contents))
          | ExpressionStringInterp e ->
              interpret_expr globals e >>= fun globals v ->
              let s = Runtime.to_s v in
              Buffer.add_string buf s;
              (globals, First ()))
      >>= fun globals () ->
      (globals, First (Runtime.String (Buffer.contents buf)))
  | Bool (b, _) -> (globals, First (Runtime.Bool b))
  | Null _ -> (globals, First Runtime.Null)
  | List (els, _) ->
      List.fold els ~init:(globals, First []) ~f:(fun globals cur ->
          globals >>= fun globals prev ->
          interpret_expr globals cur >>= fun globals el ->
          ( globals,
            First
              ((* This reverses the order *)
               el :: prev) ))
      >>= fun globals reversed_elements ->
      let els = List.rev reversed_elements in
      let arr = Array.of_list els in
      (globals, First (Runtime.List arr))
  | HashMap (kvps, _) ->
      List.fold kvps ~init:(globals, First []) ~f:(fun acc (k, v) ->
          acc >>= fun globals prev ->
          interpret_expr globals k >>= fun globals k ->
          interpret_expr globals v >>= fun globals v ->
          let cur = (k, v) in
          (* Order doesn't matter here *)
          (* TODO: we could already populate the Hashtbl here *)
          (globals, First (cur :: prev)))
      >>= fun globals kvps' ->
      let tbl = Stdlib.Hashtbl.create 8 in
      List.iter kvps' ~f:(fun (k, v) -> Stdlib.Hashtbl.add tbl k v);
      (globals, First (Runtime.HashMap tbl))
  | Subscript (receiver, subscript, pos) -> (
      interpret_expr globals receiver >>= fun globals receiver' ->
      interpret_expr globals subscript >>= fun globals subscript' ->
      match receiver' with
      | Runtime.List elements -> (
          match subscript' with
          | Runtime.Num idx ->
              if Float.is_integer idx then
                let i = Stdlib.int_of_float idx in
                (globals, First (Array.get elements i))
              else
                Printf.sprintf
                  "Lists can only be subscripted by integers, you used %s"
                  (Runtime.to_s subscript')
                |> fail ~globals pos
          | _ ->
              fail ~globals pos
                (Runtime.to_s subscript'
                |> Printf.sprintf
                     "Lists can only be subscripted by Numbers, you used %s"))
      | Runtime.HashMap tbl ->
          let result =
            match Stdlib.Hashtbl.find_opt tbl subscript' with
            | Some v -> v
            | None -> Runtime.Null
          in
          (globals, First result)
      | _ ->
          Printf.sprintf "Cannot subscript the value %s"
            (Runtime.to_s receiver')
          |> fail ~globals pos)
  | ContextId (i, pos) -> (
      match Context.get globals.context_ids i with
      | Some v -> (globals, First v)
      | None ->
          Printf.sprintf "The name %s has not been declared in this scope" i
          |> fail ~globals pos)
  | IdRef (i, pos) -> (
      match Identifiers.get globals.identifiers i with
      | Some v -> (globals, First v)
      | None ->
          Printf.sprintf "The name %s has not been declared in this scope" i
          |> fail ~globals pos)
  | Equality (lhs, rhs, is_equality, _) ->
      interpret_expr globals lhs >>= fun globals lhs ->
      interpret_expr globals rhs >>= fun globals rhs ->
      (globals, First (Runtime.Bool (is_equal globals is_equality lhs rhs)))
  | Binary (lhs, rhs, op, pos) -> interpret_binary globals lhs rhs op pos
  | MethodInvoc { receiver; target; args; pos } ->
      interpret_method ~globals ~pos receiver args target
  | FuncInvoc (receiver, args, pos) -> (
      interpret_expr globals receiver >>= fun globals -> function
      | Func f -> (
          match f with
          | User { parameters; block; identifiers } ->
              let identifiers2 = Identifiers.push_empty identifiers in
              (* Bind args to env *)
              let or_unequal =
                List.fold2 parameters args ~init:(globals, First ())
                  ~f:(fun acc p a ->
                    acc >>= fun globals () ->
                    interpret_expr globals a >>= fun globals arg_val ->
                    (* This must not throw *)
                    Identifiers.bind identifiers2 p arg_val |> Option.value_exn;
                    (globals, First ()))
              in
              (match or_unequal with
              | Ok tuple -> tuple
              | Unequal_lengths ->
                  Printf.sprintf
                    "You passed %d arguments to a function that expected %d"
                    (List.length args) (List.length parameters)
                  |> fail ~globals pos)
              >>= fun globals () ->
              let temp_globals = { globals with identifiers = identifiers2 } in
              let rec traverse_stmts globals stmts =
                match stmts with
                | [] -> (globals, First Runtime.Null)
                | hd :: tl -> (
                    let globals, either = interpret_stmt globals hd in
                    match either with
                    | First return_val ->
                        if List.is_empty tl then (globals, First return_val)
                        else (traverse_stmts [@tailrec]) globals tl
                    | Second (bt, return_val) as either -> (
                        match bt with
                        | Return -> (globals, First return_val)
                        | _ -> (globals, either)))
              in
              (* discard context *)
              (* Note, Return has already been unwrapped *)
              let _, either = traverse_stmts temp_globals block in
              (globals, either)
          | Native { cb; parameters = _; identifiers = _ } ->
              List.fold args ~init:(globals, First []) ~f:(fun acc arg ->
                  acc >>= fun globals prev ->
                  interpret_expr globals arg >>= fun globals arg ->
                  (* This is reversed... *)
                  (globals, First (arg :: prev)))
              >>= fun globals reversed_args ->
              let args = List.rev reversed_args in

              (globals, invoke_native_func ~globals ~pos cb args))
      | Method (receiver, func_t) -> (
          match func_t with
          | Native { cb; parameters = _; identifiers = _ } ->
              List.fold args ~init:(globals, First []) ~f:(fun acc arg ->
                  acc >>= fun globals prev ->
                  interpret_expr globals arg >>= fun globals arg ->
                  (* This is reversed... *)
                  (globals, First (arg :: prev)))
              >>= fun globals reversed_args ->
              let args = List.rev reversed_args in

              (globals, invoke_native_func ~globals ~pos cb (receiver :: args))
          | User _ ->
              (* I think this is unreachable... *)
              Sloth_common.Common.internal_failure __LOC__)
      | Prototype { name } -> (
          if not @@ phys_equal (List.length args) 1 then
            Printf.sprintf "TODO %s" __LOC__ |> failwith
          else
            let arg_expr = List.hd_exn args in
            interpret_expr globals arg_expr >>= fun globals arg ->
            match name with
            | "File" ->
                (globals, First (Runtime.File (cast_to_file ~globals ~pos arg)))
            | "Directory" ->
                ( globals,
                  First
                    (Runtime.Directory (cast_to_directory ~globals ~pos arg)) )
            | "Process" ->
                let either =
                  cast_to_process ~globals ~pos arg
                  |> Either.map
                       ~first:(fun proc -> Runtime.Process proc)
                       ~second:Fun.id
                in
                (globals, either)
            | _ -> Sloth_common.Common.internal_failure __LOC__)
      | _ as t ->
          Printf.sprintf "Tried to invoke %s, but it is not a function"
            (Runtime.to_s t)
          |> failwith)
  | FuncExpr { parameters; block; _ } ->
      let parameters = List.map parameters ~f:(fun (name, _) -> name) in
      let u =
        Runtime.User { parameters; block; identifiers = globals.identifiers }
      in
      let f = Runtime.Func u in
      (globals, First f)
  | IfExpr (cond, _) -> interpret_cond globals cond
  | UnaryExpr { target; pos; operator } -> (
      interpret_expr globals target >>= fun globals v ->
      match operator with
      | Not -> (
          let bool_opt = Runtime.bool_of_val v in
          match bool_opt with
          | None ->
              Runtime.to_s v
              |> Printf.sprintf
                   "The `not` operator must be applied to a Bool value, but \
                    got %s"
              |> fail ~globals pos
          | Some b -> (globals, First (Runtime.Bool (not b))))
      | Bang ->
          interpret_expr globals target >>= fun globals target ->
          let either =
            cast_to_process ~globals ~pos target
            |> Either.map
                 ~first:(fun proc ->
                   let module M = (val globals.l : Native.Sig) in
                   (* TODO add error handling *)
                   let env =
                     Context.get globals.context_ids "$env"
                     |> Option.value_exn |> Runtime.env_of_val
                     |> Option.value_exn
                   in
                   match M.proc_exec proc env with
                   | Ok t' -> t'
                   | Error err -> fail ~globals pos err)
                 ~second:Fun.id
          in
          (globals, either)
      | LeftArrow ->
          interpret_expr globals target >>= fun globals target ->
          let file = cast_to_file ~globals ~pos target in
          let target = Runtime.File file in
          let func = dereference_object globals target "readString" pos in
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
          (globals, invoke_native_func ~globals ~pos cb [ receiver; target ])
      | Minus ->
          interpret_expr globals target >>= fun globals target ->
          let f_opt = Runtime.num_of_val target in
          let f =
            match f_opt with
            | Some f -> f
            | None ->
                Printf.sprintf
                  "Unary `-` must be used with a `Number`, received %s"
                  (Runtime.to_s target)
                |> fail ~globals pos
          in
          (globals, First (Runtime.Num (Float.neg f)))
      | Plus | Product | Divide | Pipe | Less | Greater | Leq | Geq | RightArrow
      | And | Or ->
          (* Unreachable *) Sloth_common.Common.internal_failure __LOC__)
  | DoBlock (block, _) ->
      let inner_globals =
        {
          globals with
          identifiers = Identifiers.push_empty globals.identifiers;
        }
      in
      (globals, interpret_block inner_globals block)
  | ObjDeref (receiver, target, pos) ->
      interpret_expr globals receiver >>= fun globals receiver ->
      (globals, First (dereference_object globals receiver target pos))
  | LetExpr (id, e, pos) ->
      interpret_expr globals e >>= fun globals v ->
      (match Identifiers.bind globals.identifiers id v with
      | Some () -> ()
      | None ->
          Printf.sprintf
            "The name %s has already been declared in this scope; did you mean \
             to assign to it?"
            id
          |> fail ~globals pos);
      (globals, First v)
  | AssignExpr (id, e, pos) -> (
      interpret_expr globals e >>= fun globals v ->
      match Identifiers.reassign globals.identifiers id v with
      | Some () -> (globals, First v)
      | None ->
          Printf.sprintf
            "The name %s has not been declared yet; did you mean to declare it?"
            id
          |> fail ~globals pos)
  | SubAssignExpr { subscript; value; pos = _ } -> (
      match subscript with
      | Subscript (receiver, subscript, pos) -> (
          interpret_expr globals receiver >>= fun globals receiver' ->
          interpret_expr globals subscript >>= fun globals subscript' ->
          interpret_expr globals value >>= fun globals value' ->
          match receiver' with
          | HashMap tbl ->
              Stdlib.Hashtbl.replace tbl subscript' value';
              (globals, First value')
          | List elements -> (
              match Runtime.int_of_val subscript' with
              | Some i ->
                  Array.set elements i value';
                  (globals, First receiver')
              | None ->
                  Printf.sprintf
                    "Lists can only be subscripted with Numbers, instead \
                     received %s"
                    (Runtime.to_s subscript')
                  |> fail ~globals pos)
          | _ ->
              Printf.sprintf "Assigning via subscript to %s not implemented"
                (Runtime.to_s receiver')
              |> fail ~globals pos)
      | _ -> Sloth_common.Common.internal_failure __LOC__)
  | ForLoop (init, cmp, inc, bl, pos) ->
      let _, either =
        let identifiers = Identifiers.push_empty globals.identifiers in
        let globals' = { globals with identifiers } in
        let globals'', either = interpret_expr globals' init in
        (match either with
        | Second (bt, _) -> (
            match bt with
            | Exit _ | Error _ -> ()
            | Continue | Break | Return ->
                (* TODO optimizer should check for this *)
                Sloth_common.Common.internal_failure __LOC__)
        | First _ -> ());
        (globals'', either) >>= fun globals _ ->
        let rec interpret_for_loop globals cmp inc bl (last_val : Runtime.t) =
          let globals, bt_either = interpret_expr globals cmp in
          ( globals,
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
          >>= fun globals cmp_val ->
          match Runtime.bool_of_val cmp_val with
          | Some cmp_val -> (
              if not cmp_val then (globals, First last_val)
              else
                (* Each iteration should have its own scope *)
                let inner_globals =
                  Globals.
                    {
                      globals with
                      identifiers = Identifiers.push_empty globals.identifiers;
                    }
                in

                let recurse ret_val =
                  (* this iteration had no breaking stmt *)
                  let globals, either = interpret_expr globals inc in
                  match either with
                  | First _ ->
                      (interpret_for_loop [@tailcall]) globals cmp inc bl
                        ret_val
                  | Second (bt, _) -> (
                      match bt with
                      | Return ->
                          (* This is reachable if the expression was a do block *)
                          (globals, either)
                      | _ ->
                          Printf.sprintf
                            "TODO: figure out how to handle break/continue \
                             within for loop increment %s"
                            __LOC__
                          |> failwith)
                in

                match interpret_block inner_globals bl with
                | First v ->
                    (* The inner block cannot bind new names accessible out here *)
                    recurse v
                | Second (bt, break_val) as either -> (
                    match bt with
                    | Return ->
                        (* let returns bubble up *)
                        (globals, either)
                    | Break ->
                        (* Done with loop, return break_val *)
                        (globals, First break_val)
                    | Continue -> recurse break_val
                    | Error _ | Exit _ -> (globals, either)))
          | None ->
              Printf.sprintf
                "The comparison of a for loop must be a Boolean value, instead \
                 received %s"
                (Runtime.to_s cmp_val)
              |> fail ~globals pos
        in
        let _, either = interpret_for_loop globals cmp inc bl Runtime.Null in
        (globals, either)
      in
      (globals, either)
      (* for <iterator_name> in <iteratee> { <block> } *)
  | ForInLoop { iterator_name; iteratee; block; pos } ->
      let globals =
        {
          globals with
          identifiers = Identifiers.push_empty globals.identifiers;
        }
      in
      interpret_expr globals iteratee >>= fun globals iteratee ->
      let iteratee_array =
        match iteratee with
        | List l -> l
        | _ ->
            Printf.sprintf "Cannot iterate over a %s"
              (Runtime.to_class_name iteratee)
            |> fail ~globals pos
      in
      ( globals,
        Array.fold iteratee_array ~init:(First Runtime.Null)
          ~f:(fun prev element ->
            if Either.is_second prev then prev
            else
              let temp_globals =
                {
                  globals with
                  identifiers = Identifiers.push_empty globals.identifiers;
                }
              in
              Identifiers.bind temp_globals.identifiers iterator_name element
              |> Option.value_exn;
              interpret_block temp_globals block) )
      >>= fun _ ret_val -> (globals, First ret_val)
  | WithExpr (assignments, block, pos) ->
      let module M = (val globals.l) in
      let post_block_hook = ref None in
      let inner_globals =
        { globals with context_ids = Context.push_empty globals.context_ids }
      in
      (* Process side effects for certain context variables *)
      let middleware name prev next =
        match name with
        | "$cwd" ->
            (* TODO: catch type error *)
            (match Runtime.string_of_val next with
            | Some s -> M.chdir s
            | None ->
                fail ~globals pos
                @@ Printf.sprintf
                     "cannot assign %s to $cwd, it must be a String"
                @@ Runtime.to_s next);

            post_block_hook :=
              Some
                (fun () ->
                  Runtime.string_of_val prev |> Option.value_exn |> M.chdir)
        | _ -> ()
      in

      let _, either =
        List.fold assignments ~init:(inner_globals, First ())
          ~f:(fun prev (name, expr) ->
            prev >>= fun globals () ->
            interpret_expr globals expr >>= fun globals v ->
            let prev =
              match Context.get globals.context_ids name with
              | Some prev -> prev
              | None ->
                  Printf.sprintf "The context variable named %s is not defined"
                    name
                  |> fail ~globals pos
            in
            middleware name prev v;
            (match Context.reassign globals.context_ids name v with
            | Some () -> ()
            | None -> Sloth_common.Common.internal_failure __LOC__);
            (globals, First ()))
        >>= fun globals () -> (globals, interpret_block globals block)
      in
      (* This needs to run regardless of either's state *)
      let _ = Option.map !post_block_hook ~f:(fun hook -> hook ()) in
      (globals, either)

(** You must push an empty env frame on first *)
and interpret_block (globals : Globals.t) (stmts : Compiler.Optimizer.stmt list)
    : (Runtime.t, Compiler.Ast.breaking_type * Runtime.t) Either.t =
  (* TODO can't use List.fold_left because we want to handle empty list
     differently *)
  let rec traverse_stmts globals' stmts =
    match stmts with
    | [] -> First Runtime.Null
    | hd :: tl -> (
        let globals'', either = interpret_stmt globals' hd in
        match either with
        | Second (bt, v) -> Second (bt, v)
        | First v ->
            if List.is_empty tl then First v
            else (traverse_stmts [@tailcall]) globals'' tl)
  in
  traverse_stmts globals stmts

and interpret_method ~globals ~pos receiver args method_name =
  interpret_expr globals receiver >>= fun globals receiver ->
  List.fold args ~init:(globals, First []) ~f:(fun acc cur ->
      acc >>= fun globals prev ->
      interpret_expr globals cur >>= fun globals arg ->
      (* This is reversed... *)
      (globals, First (arg :: prev)))
  >>= fun globals reversed_args ->
  let args = List.rev reversed_args in
  let class_name = Runtime.to_class_name receiver in
  let klass =
    match Hashtbl.find globals.classes class_name with
    | None -> Sloth_common.Common.internal_failure __LOC__
    | Some klass -> klass
  in
  match Hashtbl.find klass.instance_members method_name with
  | None ->
      Printf.sprintf "The class %s does not have an instance field named %s"
        class_name method_name
      |> fail ~globals pos
  | Some func -> (
      match func with
      | Func func -> (
          match func with
          | User _ -> Sloth_common.Common.internal_failure __LOC__
          | Native { cb; _ } ->
              let args = receiver :: args in
              (globals, invoke_native_func ~globals ~pos cb args))
      | _ ->
          Printf.sprintf "Internal error: %s\n\n%s"
            (Runtime.to_class_name func)
            __LOC__
          |> failwith)

and is_equal globals is_equality lhs rhs =
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
          let is_deep_equal = Array.equal (is_equal globals true) lhs rhs in
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
                  | Some right_value ->
                      is_equal globals true left_value right_value)
              lhs true
          in
          Bool.(is_deep_equal = is_equality)
    | Null -> ( match rhs with Null -> is_equality | _ -> not is_equality)
    | _ ->
        Printf.sprintf "is_equal the type %s is not implemented" lh_s
        |> failwith

and dereference_object globals receiver target pos =
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
  match Hashtbl.find globals.classes class_name with
  | None ->
      Printf.sprintf
        "Internal error: could not find prototype for the %s class (%s)"
        class_name __LOC__
      |> fail ~globals pos
  | Some klass -> (
      match Hashtbl.find (table_thunk klass) target with
      | None ->
          Printf.sprintf "The class %s does not have a %s field named %s"
            class_name descriptor target
          |> fail ~globals pos
      | Some field -> (
          match field with
          | Func func_t -> Method (receiver, func_t)
          | _ -> fail ~globals pos "TODO"))

and cast_to_directory ~globals ~pos = function
  | Runtime.String path -> path
  | _ as t' ->
      fail ~globals pos
      @@ Printf.sprintf "There is no way to cast from a %s to a Directory"
      @@ Runtime.to_s t'

and cast_to_file ~globals ~pos = function
  | Runtime.File f -> f
  | Runtime.String path -> { path }
  | _ as t' ->
      fail ~globals pos
      @@ Printf.sprintf "There is no way to cast from a %s to a File"
      @@ Runtime.to_s t'

and cast_to_process ~globals ~pos v :
    (Runtime.process, Compiler.Ast.breaking_type * Runtime.t) Either.t =
  match v with
  | Runtime.Process p -> First p
  | Runtime.List _ as l -> (
      let constructor =
        dereference_object globals
          (Runtime.Prototype { name = "Process" })
          "new" pos
      in
      let callback =
        match constructor with
        | Func func_t -> (
            match func_t with
            | Native { cb; _ } ->
                fun () -> invoke_native_func ~globals ~pos cb [ l ]
            | User _ -> Sloth_common.Common.internal_failure __LOC__)
        | Method (receiver, func_t) -> (
            match func_t with
            | User _ -> Sloth_common.Common.internal_failure __LOC__
            | Native { cb; _ } ->
                fun () -> invoke_native_func ~globals ~pos cb [ receiver; l ])
        | _ -> Sloth_common.Common.internal_failure __LOC__
      in
      match callback () with
      | Second _ as second -> second
      | First proc -> (cast_to_process [@tailcall]) ~globals ~pos proc)
  | Runtime.String s ->
      let list =
        shell_like_escape s
        |> List.map ~f:(fun s -> Runtime.String s)
        |> Array.of_list
      in
      (cast_to_process [@tailcall]) ~globals ~pos (Runtime.List list)
  | _ as t' ->
      fail ~globals pos
      @@ Printf.sprintf "Expected a Process, but got a %s"
      @@ Runtime.to_s t'

and interpret_binary globals lhs rhs op pos =
  interpret_expr globals lhs >>= fun globals lhs ->
  let cast_to_number = function
    | Runtime.Num f -> f
    | Runtime.String s -> (
        match Float.of_string_opt s with
        | Some f -> f
        | None ->
            fail ~globals pos
            @@ Printf.sprintf "Expected a Number, but got the string \"%s\"" s)
    | _ as t' ->
        fail ~globals pos
        @@ Printf.sprintf "Expected a Number, but got a %s"
        @@ Runtime.to_s t'
  in
  match op with
  | Pipe ->
      let ( >>- ) (globals, either) callback =
        match either with
        | Second _ as second -> (globals, second)
        | First t -> callback t
      in
      interpret_expr globals rhs >>= fun globals rhs ->
      (globals, cast_to_process ~globals ~pos lhs) >>- fun left ->
      (globals, cast_to_process ~globals ~pos rhs) >>- fun right ->
      let read, write = Core_unix.pipe () in
      left.stdout <- write;
      left.pipes_to_collect <- write :: left.pipes_to_collect;
      right.stdin <- read;
      right.pipes_to_collect <- read :: right.pipes_to_collect;
      let right = { right with previous = Some left } in
      (globals, Either.first @@ Runtime.Process right)
  | Plus ->
      interpret_expr globals rhs >>= fun globals rhs ->
      let left = cast_to_number lhs in
      let right = cast_to_number rhs in
      (globals, Either.first @@ Runtime.Num (left +. right))
  | Minus ->
      interpret_expr globals rhs >>= fun globals rhs ->
      let left = cast_to_number lhs in
      let right = cast_to_number rhs in
      (globals, Either.first @@ Runtime.Num (left -. right))
  | Divide ->
      interpret_expr globals rhs >>= fun globals rhs ->
      let left = cast_to_number lhs in
      let right = cast_to_number rhs in
      (globals, Either.first @@ Runtime.Num (left /. right))
  | Product ->
      interpret_expr globals rhs >>= fun globals rhs ->
      let left = cast_to_number lhs in
      let right = cast_to_number rhs in
      (globals, Either.first @@ Runtime.Num (left *. right))
  | And -> (
      let left =
        match Runtime.bool_of_val lhs with
        | None ->
            Printf.sprintf "Expected a `Bool`, but got %s" (Runtime.to_s lhs)
            |> fail ~globals pos
        | Some b -> b
      in
      if Bool.(left = false) then (globals, Either.first @@ Runtime.Bool false)
      else
        interpret_expr globals rhs >>= fun globals rhs ->
        match Runtime.bool_of_val rhs with
        | None ->
            Printf.sprintf "Expected a `Bool`, but got %s" (Runtime.to_s rhs)
            |> fail ~globals pos
        | Some b -> (globals, Either.first @@ Runtime.Bool b))
  | Or -> (
      let left =
        match Runtime.bool_of_val lhs with
        | None ->
            Printf.sprintf "Expected a `Bool`, but got %s" (Runtime.to_s lhs)
            |> fail ~globals pos
        | Some b -> b
      in
      if Bool.(left = true) then (globals, Either.first @@ Runtime.Bool true)
      else
        interpret_expr globals rhs >>= fun globals rhs ->
        match Runtime.bool_of_val rhs with
        | None ->
            Printf.sprintf "Expected a `Bool`, but got %s" (Runtime.to_s rhs)
            |> fail ~globals pos
        | Some b -> (globals, Either.first @@ Runtime.Bool b))
  | Leq ->
      interpret_expr globals rhs >>= fun globals rhs ->
      let left = cast_to_number lhs in
      let right = cast_to_number rhs in
      (globals, Either.first @@ Runtime.Bool Float.(left <= right))
  | Geq ->
      interpret_expr globals rhs >>= fun globals rhs ->
      let left = cast_to_number lhs in
      let right = cast_to_number rhs in
      (globals, Either.first @@ Runtime.Bool Float.(left >= right))
  | Less ->
      interpret_expr globals rhs >>= fun globals rhs ->
      let left = cast_to_number lhs in
      let right = cast_to_number rhs in
      (globals, Either.first @@ Runtime.Bool Float.(left < right))
  | Greater ->
      interpret_expr globals rhs >>= fun globals rhs ->
      let left = cast_to_number lhs in
      let right = cast_to_number rhs in
      (globals, Either.first @@ Runtime.Bool Float.(left > right))
  | RightArrow ->
      (*
        TODO: consider:
          - directly write a string: `"Hello" -> "hello.txt"`
          - write output of a proc: `Process.new("uname") -> "os.txt"` (this should be optimized)
      *)
      let left = cast_to_string ~globals ~pos lhs in
      interpret_expr globals rhs >>= fun globals rhs ->
      let Runtime.{ path } = cast_to_file ~globals ~pos rhs in
      let module M = (val globals.l) in
      M.file_write_all path ~data:left;
      (globals, Either.first @@ Runtime.String left)
  | Bang | Not | LeftArrow ->
      (* Not binary ops, unreachable *)
      Sloth_common.Common.internal_failure __LOC__

and cast_to_string ~globals ~pos t' =
  let open Runtime in
  match t' with
  | String s -> s
  | ProcessResult { stdout; _ } -> stdout
  | Process proc -> (
      let module M = (val globals.l : Native.Sig) in
      let env_val =
        Context.get globals.context_ids "$env" |> Option.value_exn
      in
      let env =
        match Runtime.env_of_val env_val with
        | Some env -> env
        | None ->
            Printf.sprintf "$env is the wrong type: %s" @@ Runtime.to_s env_val
            |> fail ~globals pos
      in
      match M.proc_exec proc env with
      | Ok t' -> (cast_to_string [@tailcall]) ~globals ~pos t'
      | Error err -> fail ~globals pos err)
  | _ as t' ->
      fail ~globals pos
      @@ Printf.sprintf "Expected a String, but got a %s"
      @@ Runtime.to_s t'
