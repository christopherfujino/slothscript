open Core
open Common

let ( >>= ) =
 fun (globals, either) cb ->
  Either.value_map either
    ~second:(fun tuple -> (globals, Second tuple))
    ~first:(cb globals)

let failure ~globals pos msg =
  let pos_s = Sloth_common.Position.string_of_t pos in
  let msg1 =
    Printf.sprintf "%s\n\n[%s] Runtime error: %s"
      (Sloth_common.Position.summarize pos Globals.(globals.src))
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

let rec interpret_prog globals prog =
  match prog with
  | [] -> (globals, Runtime.Null)
  | hd :: tl -> (
      let new_globals, v = interpret_decl globals hd in
      match tl with
      | [] -> (globals, v) (* TODO is this right? *)
      | _ -> (interpret_prog [@tailcall]) new_globals tl)

and interpret_decl (globals : Globals.t) decl =
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
               identifiers = globals.identifiers;
             })
      in
      (match Identifiers.bind globals.identifiers name f with
      | Some () -> ()
      | None ->
          Printf.sprintf "A function named %s has already been declared" name
          |> failure ~globals pos);
      (globals, f)
  | StmtDecl s ->
      let globals, either = interpret_stmt globals s in
      let v =
        match either with
        | First v -> v
        | Second (bt, _) -> (
            match bt with
            | Break | Continue | Return ->
                (* TODO: This should be caught by optimizer *)
                Sloth_common.Common.internal_failure __LOC__)
      in
      (globals, v)

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
              { globals with identifiers = Identifiers.push_empty globals.identifiers }
            in
            (globals, interpret_block inner_globals block)
          else
            match continuation with
            | None -> (globals, First Runtime.Null (* TODO: Is this right? *))
            | Some cond -> (interpret_cond [@tailcall]) globals cond)
      | None ->
          Printf.sprintf
            "If-expressions must have a boolean expression, but you used %s"
            (Runtime.to_s condition)
          |> failure ~globals pos)
  | Compiler.Optimizer.ElseCont (stmts, _) ->
      let inner_globals =
        { globals with identifiers = Identifiers.push_empty globals.identifiers }
      in
      (globals, interpret_block inner_globals stmts)

and interpret_expr globals expr :
    Globals.t * (Runtime.t, Compiler.Ast.breaking_type * Runtime.t) Either.t =
  let open Compiler.Optimizer in
  match expr with
  | Num (f, _) -> (globals, First (Runtime.Num f))
  | String (parts, _) ->
      let buf = Buffer.create 128 in
      List.fold parts ~init:(globals, First ()) ~f:(fun (globals, either) part ->
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
      >>= fun globals () -> (globals, First (Runtime.String (Buffer.contents buf)))
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
                |> failure ~globals pos
          | _ ->
              failure ~globals pos
                (Runtime.to_s subscript'
                |> Printf.sprintf
                     "Lists can only be subscripted by Numbers, you used %s"))
      | Runtime.HashMap tbl -> (globals, First (Stdlib.Hashtbl.find tbl subscript'))
      | _ ->
          Printf.sprintf "Cannot subscript the value %s"
            (Runtime.to_s receiver')
          |> failure ~globals pos)
  | ContextId (i, pos) -> (
      match Context.get globals.context_ids i with
      | Some v -> (globals, First v)
      | None ->
          Printf.sprintf "The name %s has not been declared in this scope" i
          |> failure ~globals pos)
  | IdRef (i, pos) -> (
      match Identifiers.get globals.identifiers i with
      | Some v -> (globals, First v)
      | None ->
          Printf.sprintf "The name %s has not been declared in this scope" i
          |> failure ~globals pos)
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
                  |> failure ~globals pos)
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
          | Native { cb; parameters = _; identifiers = _ } -> (
              List.fold args ~init:(globals, First []) ~f:(fun acc arg ->
                  acc >>= fun globals prev ->
                  interpret_expr globals arg >>= fun globals arg ->
                  (* This is reversed... *)
                  (globals, First (arg :: prev)))
              >>= fun globals reversed_args ->
              let args = List.rev reversed_args in

              match cb args with
              | Ok v -> (globals, First v)
              | Error msg ->
                  (* TODO: should this return a `Some SlothError`? *)
                  failure ~globals pos msg))
      | Method (receiver, func_t) -> (
          match func_t with
          | Native { cb; parameters = _; identifiers = _ } -> (
              List.fold args ~init:(globals, First []) ~f:(fun acc arg ->
                  acc >>= fun globals prev ->
                  interpret_expr globals arg >>= fun globals arg ->
                  (* This is reversed... *)
                  (globals, First (arg :: prev)))
              >>= fun globals reversed_args ->
              let args = List.rev reversed_args in

              match cb (receiver :: args) with
              | Ok v -> (globals, First v)
              | Error msg ->
                  (* TODO: should this return a `Some SlothError`? *)
                  failure ~globals pos msg)
          | User _ ->
              (* I think this is unreachable... *)
              Sloth_common.Common.internal_failure __LOC__)
      | Prototype { name } -> (
          if not @@ phys_equal (List.length args) 1 then failwith "TODO"
          else
            let arg_expr = List.hd_exn args in
            interpret_expr globals arg_expr >>= fun globals arg ->
            match name with
            | "File" -> (globals, First (Runtime.File (cast_to_file ~globals ~pos arg)))
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
              |> failure ~globals pos
          | Some b -> (globals, First (Runtime.Bool (not b))))
      | Bang -> (
          interpret_expr globals target >>= fun globals target ->
          let proc = cast_to_process ~globals ~pos target in
          match Globals.exec_proc proc with
          | Ok t' -> (globals, First t')
          | Error err -> failure ~globals pos err)
      | LeftArrow -> (
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
          match cb [ receiver; target ] with
          | Error err -> failure ~globals pos err
          | Ok t' -> (globals, First t'))
      | Plus | Minus | Product | Divide | Pipe | Less | Greater | Leq | Geq
      | RightArrow ->
          (* Unreachable *) Sloth_common.Common.internal_failure __LOC__)
  | DoBlock (block, _) ->
      let inner_globals =
        { globals with identifiers = Identifiers.push_empty globals.identifiers }
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
          |> failure ~globals pos);
      (globals, First v)
  | AssignExpr (id, e, pos) -> (
      interpret_expr globals e >>= fun globals v ->
      match Identifiers.reassign globals.identifiers id v with
      | Some () -> (globals, First v)
      | None ->
          Printf.sprintf
            "The name %s has not been declared yet; did you mean to declare it?"
            id
          |> failure ~globals pos)
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
                    "Lists can only be subscripted with Numbers, but you used \
                     %s"
                    (Runtime.to_s subscript')
                  |> failure ~globals pos)
          | _ ->
              Printf.sprintf "Assigning via subscript to %s not implemented"
                (Runtime.to_s receiver')
              |> failure ~globals pos)
      | _ -> Sloth_common.Common.internal_failure __LOC__)
  | ForLoop (init, cmp, inc, bl, pos) ->
      let _, either =
        let identifiers = Identifiers.push_empty globals.identifiers in
        let globals' = { globals with identifiers } in
        let globals'', either = interpret_expr globals' init in
        (match either with
        | Second (bt, _) -> (
            match bt with
            | Continue | Break | Return ->
                (* TODO optimizer should check for this *)
                Sloth_common.Common.internal_failure __LOC__)
        | First _ -> (globals'', Either.First Runtime.Null))
        >>= fun globals _ ->
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
                      (interpret_for_loop [@tailcall]) globals cmp inc bl ret_val
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
                    | Continue -> recurse break_val))
          | None ->
              Printf.sprintf
                "The comparison of a for loop must be a Boolean value, but you \
                 used %s"
                (Runtime.to_s cmp_val)
              |> failure ~globals pos
        in
        let _, either = interpret_for_loop globals cmp inc bl Runtime.Null in
        (globals, either)
      in
      (globals, either)
      (* for <iterator_name> in <iteratee> { <block> } *)
  | ForInLoop { iterator_name; iteratee; block; pos } ->
      let globals =
        { globals with identifiers = Identifiers.push_empty globals.identifiers }
      in
      interpret_expr globals iteratee >>= fun globals iteratee ->
      let iteratee_array =
        match iteratee with
        | List l -> l
        | _ ->
            Printf.sprintf "Cannot iterate over a %s"
              (Runtime.to_class_name iteratee)
            |> failure ~globals pos
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
                |> failure ~globals:globals pos);
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
and interpret_block (globals : Globals.t) (stmts : Compiler.Optimizer.stmt list) :
    (Runtime.t, Compiler.Ast.breaking_type * Runtime.t) Either.t =
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
      |> failure ~globals pos
  | Some func -> (
      match func with
      | Func func -> (
          match func with
          | User _ -> Sloth_common.Common.internal_failure __LOC__
          | Native { cb; _ } -> (
              let args = receiver :: args in
              match cb args with
              | Ok v -> (globals, First v)
              | Error msg ->
                  (* TODO propagate SlothError *) failure ~globals pos msg))
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
                  | Some right_value -> is_equal globals true left_value right_value)
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
      |> failure ~globals pos
  | Some klass -> (
      match Hashtbl.find (table_thunk klass) target with
      | None ->
          Printf.sprintf "The class %s does not have a %s field named %s"
            class_name descriptor target
          |> failure ~globals pos
      | Some field -> (
          match field with
          | Func func_t -> Method (receiver, func_t)
          | _ -> failure ~globals pos "TODO"))

and cast_to_file ~globals ~pos = function
  | Runtime.File f -> f
  | Runtime.String filename -> (
      let intermediate =
        dereference_object globals (Runtime.Prototype { name = "File" }) "new" pos
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
      | Ok file -> (cast_to_file [@tailcall]) ~globals ~pos file
      | Error err -> failure ~globals pos err)
  | _ as t' ->
      failure ~globals pos
      @@ Printf.sprintf "Expected a File but got a %s"
      @@ Runtime.to_s t'

and cast_to_process ~globals ~pos = function
  | Runtime.Process p -> p
  | Runtime.List _ as l ->
      let constructor =
        dereference_object globals
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
        | Error err -> failure ~globals pos err
        | Ok proc -> proc
      in
      (cast_to_process [@tailcall]) ~globals ~pos proc
  | Runtime.String s ->
      let list =
        shell_like_escape s
        |> List.map ~f:(fun s -> Runtime.String s)
        |> Array.of_list
      in
      (cast_to_process [@tailcall]) ~globals ~pos (Runtime.List list)
  | _ as t' ->
      failure ~globals pos
      @@ Printf.sprintf "Expected a Process, but got a %s"
      @@ Runtime.to_s t'

and interpret_binary globals lhs rhs op pos =
  interpret_expr globals lhs >>= fun globals lhs ->
  interpret_expr globals rhs >>= fun globals rhs ->
  let cast_to_number = function
    | Runtime.Num f -> f
    | Runtime.String s -> (
        match Float.of_string_opt s with
        | Some f -> f
        | None ->
            failure ~globals pos
            @@ Printf.sprintf "Expected a Number, but got the string \"%s\"" s)
    | _ as t' ->
        failure ~globals pos
        @@ Printf.sprintf "Expected a Number, but got a %s"
        @@ Runtime.to_s t'
  in
  match op with
  | Pipe ->
      let left = cast_to_process ~globals ~pos lhs in
      let right = cast_to_process ~globals ~pos rhs in
      let read, write = Core_unix.pipe () in
      left.stdout <- write;
      left.pipes_to_collect <- write :: left.pipes_to_collect;
      right.stdin <- read;
      right.pipes_to_collect <- read :: right.pipes_to_collect;
      let right = { right with previous = Some left } in
      (globals, Either.first @@ Runtime.Process right)
  | Plus ->
      let left = cast_to_number lhs in
      let right = cast_to_number rhs in
      (globals, Either.first @@ Runtime.Num (left +. right))
  | Minus ->
      let left = cast_to_number lhs in
      let right = cast_to_number rhs in
      (globals, Either.first @@ Runtime.Num (left -. right))
  | Divide ->
      let left = cast_to_number lhs in
      let right = cast_to_number rhs in
      (globals, Either.first @@ Runtime.Num (left /. right))
  | Product ->
      let left = cast_to_number lhs in
      let right = cast_to_number rhs in
      (globals, Either.first @@ Runtime.Num (left *. right))
  | Leq ->
      let left = cast_to_number lhs in
      let right = cast_to_number rhs in
      (globals, Either.first @@ Runtime.Bool Float.(left <= right))
  | Geq ->
      let left = cast_to_number lhs in
      let right = cast_to_number rhs in
      (globals, Either.first @@ Runtime.Bool Float.(left >= right))
  | Less ->
      let left = cast_to_number lhs in
      let right = cast_to_number rhs in
      (globals, Either.first @@ Runtime.Bool Float.(left < right))
  | Greater ->
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
      let Runtime.{ path } = cast_to_file ~globals ~pos rhs in
      Out_channel.write_all path ~data:left;
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
      match Globals.exec_proc proc with
      | Ok t' -> (cast_to_string [@tailcall]) ~globals ~pos t'
      | Error err -> failure ~globals pos err)
  | _ as t' ->
      failure ~globals pos
      @@ Printf.sprintf "Expected a String, but got a %s"
      @@ Runtime.to_s t'
