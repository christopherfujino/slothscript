open Core
open Common
open Sloth_common.Common

(** Bind an either and globals where globals need to be passed. *)
let ( >>= ) (globals, either) cb =
  match either with
  | First t -> cb globals t
  | Second _ as second -> (globals, second)

(** Bind an either and globals where the callback does not require or produce
    globals *)
let ( >>- ) either cb =
  match either with First t -> cb t | Second _ as second -> second

let abstract_failure_msg ~globals ~pos msg type_s =
  Runtime.backtrace_to_s ~pos
    Globals.(globals.stack_frames)
    globals.src msg type_s

let runtime_failure_msg ~globals ~pos msg =
  abstract_failure_msg ~globals ~pos msg "Runtime error"

let internal_failure_msg ~globals ~pos msg =
  abstract_failure_msg ~globals ~pos msg "Internal failure"

let create_error ~globals ~pos s =
  Runtime.Error
    ( Some
        (Runtime.backtrace_to_s ~pos
           Globals.(globals.stack_frames)
           globals.src s "Runtime error"),
      Runtime.String s )

(* TODO make this a private type so we don't accidentally create error
   messages without source summaries *)
let failure_obj ~globals ~pos msg = Second (create_error ~globals ~pos msg)

let fail ~globals pos msg =
  let msg = runtime_failure_msg ~globals ~pos msg in
  raise (RuntimeError msg)

let rec invoke_native_func ~globals ~pos cb args =
  (* We don't push a stack frame because if this errors capture the position
     anyway *)
  let eval ~args f =
    let arity_opt =
      match f with
      | Runtime.Native { arity; _ } -> arity
      | User { parameters; _ } -> Some (List.length parameters)
    in

    let err_opt =
      match arity_opt with
      | Some arity ->
          let arg_len = List.length args in
          if not (arity = arg_len) then
            let msg =
              Printf.sprintf
                "this function expected to take a callback taking %d arguments \
                 but received a function that takes %d"
                arg_len arity
            in
            Option.some @@ failure_obj ~globals ~pos msg
          else None
      | None -> None
    in

    match err_opt with
    | Some e -> e
    | None ->
        (* We must hide the globals type from Stdlib_impl *)
        (* TODO: Do we need to create a synthetic pos? *)
        (* Do we need to push a stack frame for native funcs? *)
        let _, either = invoke_func ~globals ~pos ~args f in
        either
  in
  let either =
    try cb Globals.(globals.context_ids) eval args
    with InternalFailure msg -> fail ~globals pos msg
  in
  match either with
  | First _ as first -> first
  | Second bt as second -> (
      match bt with
      | Runtime.Return _ | Break _ | Continue _ -> internal_failure __LOC__
      | Exit _ -> second
      | Error (_, msg) ->
          failure_obj ~globals ~pos
            (Runtime.string_of_val msg |> option_value ~message:__LOC__))

and invoke_func ~globals ~pos ~args = function
  | Runtime.User { parameters; block; identifiers; name; pos = _ } ->
      let identifiers2 = Identifiers.push_empty identifiers in
      (* Bind args to env *)
      let or_unequal =
        List.fold2 parameters args ~init:(globals, First ()) ~f:(fun acc p a ->
            acc >>= fun globals () ->
            (* This must not throw *)
            Identifiers.bind identifiers2 p a |> option_value ~message:__LOC__;
            (globals, First ()))
      in
      (match or_unequal with
        | Ok tuple -> tuple
        | Unequal_lengths ->
            (* TODO use the User.pos field *)
            Printf.sprintf
              "You passed %d arguments to a function that expected %d"
              (List.length args) (List.length parameters)
            |> fail ~globals pos)
      >>= fun globals () ->
      let temp_globals = { globals with identifiers = identifiers2 } in
      (* Note this is the invocation pos, not the func decl pos *)
      let temp_globals = Globals.push_frame temp_globals name pos in
      let rec traverse_stmts globals stmts =
        match stmts with
        | [] -> (globals, First Runtime.Null)
        | hd :: tl -> (
            let globals, either = interpret_stmt globals hd in
            match either with
            | First return_val ->
                if List.is_empty tl then (globals, First return_val)
                else (traverse_stmts [@tailrec]) globals tl
            | Second bt as either -> (
                match bt with
                | Return return_val -> (globals, First return_val)
                | _ -> (globals, either)))
      in
      (* discard context *)
      (* Note, Return has already been unwrapped *)
      let _, either = traverse_stmts temp_globals block in
      (globals, either)
  | Native { cb; name = _; arity = _ } ->
      (globals, invoke_native_func ~globals ~pos cb args)

and interpret_prog globals prog =
  let globals, either =
    List.fold prog ~init:(globals, First Runtime.Null)
      ~f:(fun (globals, either) decl ->
        (globals, either) >>= fun globals _ -> interpret_decl globals decl)
  in
  match either with
  | First _ as first -> (globals, first)
  | Second bt as second -> (
      match bt with
      | Break _ | Continue _ | Return _ -> internal_failure __LOC__
      | Exit _ | Error _ -> (globals, second))

and interpret_decl (globals : Globals.t) decl :
    Globals.t * (Runtime.t, Runtime.breaking_type) Either.t =
  let open Compiler.Optimizer in
  match decl with
  | FuncDecl { name; parameters; block; pos } ->
      let parameters = List.map parameters ~f:(fun (name, _) -> name) in
      let f =
        Runtime.Func
          (User
             { parameters; block; identifiers = globals.identifiers; name; pos })
      in
      (match Identifiers.bind globals.identifiers name f with
      | Some () -> ()
      | None ->
          (* This should be caught by optimizer *)
          internal_failure __LOC__);
      (globals, First f)
  | StmtDecl s ->
      let globals, either = interpret_stmt globals s in
      (match either with
      | First _ -> ()
      | Second bt -> (
          match bt with
          | Break _ | Continue _ | Return _ ->
              (* TODO: This should be caught by optimizer *)
              internal_failure __LOC__
          | Exit _ | Error _ -> ()));
      (globals, either)

and interpret_stmt (globals : Globals.t) stmt :
    Globals.t * (Runtime.t, Runtime.breaking_type) Either.t =
  let open Compiler.Optimizer in
  match stmt with
  | ExprStmt expr -> interpret_expr globals expr
  | BreakingStmt (break_type, expr_opt, pos) -> (
      match break_type with
      | Break -> (
          match expr_opt with
          | None -> (globals, Second (Runtime.Break Runtime.Null))
          | Some e ->
              interpret_expr globals e >>= fun globals v ->
              (globals, Second (Runtime.Break v)))
      | Return -> (
          match expr_opt with
          | None -> (globals, Second (Runtime.Return Runtime.Null))
          | Some e ->
              interpret_expr globals e >>= fun globals v ->
              (globals, Second (Runtime.Return v)))
      | Continue -> (
          match expr_opt with
          | None -> (globals, Second (Runtime.Continue Runtime.Null))
          | Some e ->
              interpret_expr globals e >>= fun globals v ->
              (globals, Second (Runtime.Continue v)))
      | Error -> (
          match expr_opt with
          | None -> (globals, failure_obj ~globals ~pos "")
          | Some e ->
              interpret_expr globals e >>= fun globals v ->
              let msg = Runtime.to_s v in
              (globals, failure_obj ~globals ~pos msg)))

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

and interpret_expr globals (expr : Compiler.Optimizer.expr) =
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
      let arr = Dynarray.of_list els in
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
                (globals, First (Dynarray.get elements i))
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
      (globals, First (Runtime.Bool (Runtime.is_equal is_equality lhs rhs)))
  | Binary (lhs, rhs, op, pos) -> interpret_binary globals lhs rhs op pos
  | MethodInvoc { receiver; target; args; pos } ->
      interpret_method ~globals ~pos receiver args target
  | FuncInvoc (receiver, args, pos) -> (
      interpret_expr globals receiver >>= fun globals -> function
      | Func f ->
          List.fold args ~init:(globals, First []) ~f:(fun acc arg ->
              acc >>= fun globals prev ->
              interpret_expr globals arg >>= fun globals arg ->
              (globals, First (arg :: prev)))
          >>= fun globals reversed_args ->
          let args = List.rev reversed_args in
          invoke_func ~globals ~pos ~args f
      | Method (receiver, func_t) -> (
          match func_t with
          | Native { cb; name = _; arity = _ } ->
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
              internal_failure __LOC__)
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
                  (* TODO This should be a separate implementation *)
                  cast_to_process ~globals ~pos arg
                  |> Either.map
                       ~first:(fun proc -> Runtime.Process proc)
                       ~second:Fun.id
                in
                (globals, either)
            | "String" -> (globals, First (Runtime.String (Runtime.to_s arg)))
            | "Number" -> (
                match arg with
                | Runtime.String s -> (
                    match Float.of_string_opt s with
                    | Some f -> (globals, First (Runtime.Num f))
                    | None ->
                        ( globals,
                          failure_obj ~globals ~pos
                            (Printf.sprintf
                               "Cannot cast the string \"%s\" to a `Number`" s)
                        ))
                | Runtime.Num n -> (globals, First (Runtime.Num n))
                | _ ->
                    ( globals,
                      failure_obj ~globals ~pos
                      @@ Printf.sprintf "Cannot cast a %s to a `Number`"
                           (Runtime.to_class_name arg) ))
            | _ ->
                internal_failure_msg ~globals ~pos
                  (Printf.sprintf "unimplemented constructor %s (%s)" name
                     __LOC__)
                |> internal_failure)
      | _ as t ->
          ( globals,
            failure_obj ~globals ~pos
              (Printf.sprintf "Tried to invoke %s, but it is not a function"
              @@ Runtime.to_s t) ))
  | FuncExpr { parameters; block; pos } ->
      let parameters = List.map parameters ~f:(fun (name, _) -> name) in
      let u =
        Runtime.User
          {
            parameters;
            block;
            identifiers = globals.identifiers;
            pos;
            name = "(anonymous)";
          }
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
      | Ampersand -> (
          interpret_expr globals target >>= fun globals target ->
          (globals, cast_to_process ~globals ~pos target)
          >>= fun globals target ->
          let target = Runtime.Process target in
          (globals, dereference_object globals target "forkBuffer" pos)
          >>= fun globals f ->
          let _, func_t =
            Runtime.method_of_val f |> option_value ~message:__LOC__
          in
          match func_t with
          | User _ ->
              internal_failure_msg ~globals ~pos __LOC__ |> internal_failure
          | Native { cb; name = _; arity = _ } ->
              (globals, invoke_native_func ~globals ~pos cb [ target ]))
      | AmpersandBang -> (
          interpret_expr globals target >>= fun globals target ->
          (globals, cast_to_process ~globals ~pos target)
          >>= fun globals target ->
          let target = Runtime.Process target in
          (globals, dereference_object globals target "blockBuffer" pos)
          >>= fun globals f ->
          let _, func_t =
            Runtime.method_of_val f |> option_value ~message:__LOC__
          in
          match func_t with
          | User _ ->
              internal_failure_msg ~globals ~pos __LOC__ |> internal_failure
          | Native { cb; name = _; arity = _ } ->
              (globals, invoke_native_func ~globals ~pos cb [ target ]))
      | Bang -> (
          interpret_expr globals target >>= fun globals target ->
          (globals, cast_to_process ~globals ~pos target)
          >>= fun globals target_proc ->
          let target = Runtime.Process target_proc in
          (globals, dereference_object globals target "blockInherit" pos)
          >>= fun globals f ->
          let _, func_t =
            Runtime.method_of_val f |> option_value ~message:__LOC__
          in
          match func_t with
          | User _ ->
              internal_failure_msg ~globals ~pos __LOC__ |> internal_failure
          | Native { cb; name = _; arity = _ } ->
              (globals, invoke_native_func ~globals ~pos cb [ target ]))
      | LeftArrow ->
          interpret_expr globals target >>= fun globals target ->
          ( globals,
            cast_to_file_descriptor ~globals ~pos ~mode:[ Core_unix.O_RDONLY ]
              ~m:globals.l target )
          >>= fun globals fd ->
          let fd = Runtime.FileDescriptor fd in
          (globals, dereference_object globals fd "readAll" pos)
          >>= fun globals func ->
          let receiver, func =
            match Runtime.method_of_val func with
            | None -> internal_failure __LOC__
            | Some func -> func
          in
          let cb =
            match func with
            | User _ -> internal_failure __LOC__
            | Native { cb; _ } -> cb
          in
          (* TODO close fd *)
          (globals, invoke_native_func ~globals ~pos cb [ receiver ])
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
      | Plus | Product | Divide | Modulo | Pipe | Less | Greater | Leq | Geq
      | RightArrow | And | Or ->
          (* Not unary operators *) internal_failure __LOC__)
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
      (globals, dereference_object globals receiver target pos)
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
      (* Don't bind _ *)
      if String.(id = "_") then (globals, First v)
      else
        match Identifiers.reassign globals.identifiers id v with
        | Some () -> (globals, First v)
        | None ->
            Printf.sprintf
              "The name %s has not been declared yet; did you mean to declare \
               it?"
              id
            |> internal_failure_msg ~globals ~pos
            |> internal_failure)
  | DerefAssign { receiver; name; value; pos } ->
      interpret_expr globals receiver >>= fun globals receiver ->
      interpret_expr globals value >>= fun globals value ->
      (globals, reassign_object_property ~globals ~pos receiver name value)
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
                  Dynarray.set elements i value';
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
      | _ -> internal_failure __LOC__)
  | ForLoop (init, cmp, inc, bl, pos) ->
      let _, either =
        let identifiers = Identifiers.push_empty globals.identifiers in
        let globals' = { globals with identifiers } in
        let globals'', either = interpret_expr globals' init in
        (match either with
        | Second bt -> (
            match bt with
            | Exit _ | Error _ -> ()
            | Continue _ | Break _ | Return _ ->
                (* TODO optimizer should check for this *)
                internal_failure __LOC__)
        | First _ -> ());
        (globals'', either) >>= fun globals _ ->
        let rec interpret_for_loop globals cmp inc bl (last_val : Runtime.t) =
          let globals, bt_either = interpret_expr globals cmp in
          ( globals,
            match bt_either with
            | First _ -> bt_either
            | Second bt -> (
                match bt with
                | Return _ ->
                    (* This is reachable if the expression was a do block with
                       a return statement in it *)
                    either
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
                  | Second bt -> (
                      match bt with
                      | Return _ ->
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
                | Second bt as either -> (
                    match bt with
                    | Return _ ->
                        (* let returns bubble up *)
                        (globals, either)
                    | Break break_val ->
                        (* Done with loop, return break_val *)
                        (globals, First break_val)
                    | Continue v -> recurse v
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
  | ForInLoop { iterator_name; iteratee; block; pos } -> (
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
      let globals, either =
        ( globals,
          Dynarray.fold_left
            (fun prev element ->
              if Either.is_second prev then prev
              else
                let temp_globals =
                  {
                    globals with
                    identifiers = Identifiers.push_empty globals.identifiers;
                  }
                in
                Identifiers.bind temp_globals.identifiers iterator_name element
                |> option_value
                     ~message:(internal_failure_msg ~globals ~pos __LOC__);
                interpret_block temp_globals block)
            (First Runtime.Null) iteratee_array )
      in
      match either with
      | First ret_val -> (globals, First ret_val)
      | Second breaking_type -> (
          match breaking_type with
          | Break v -> (globals, First v)
          | Continue v -> (globals, First v)
          | Error _ -> (globals, either)
          | Exit _ -> (globals, either)
          | Return _ -> (globals, either)))
  | WithExpr (assignments, block, pos) ->
      let module M = (val globals.l) in
      let post_block_hook = ref None in
      let inner_globals =
        { globals with context_ids = Context.push_empty globals.context_ids }
      in
      (*
      Process side effects for certain context variables
      These can't be recovered from, we must fail immediately
      *)
      let middleware name prev next =
        match name with
        | "$cwd" ->
            let cd_wrapper p =
              match M.chdir p with
              | Ok new_p -> (
                  match
                    Context.reassign globals.context_ids "$cwd"
                      (Runtime.String new_p)
                  with
                  | Some () -> Printf.printf "[DEBUG] %s\n" new_p; ()
                  | None -> internal_failure "Failed to re-assign $cwd")
              | Error msg -> fail ~globals pos msg
            in
            (match Runtime.string_of_val next with
            | Some s -> cd_wrapper s
            | None ->
                fail ~globals pos
                @@ Printf.sprintf
                     "cannot assign %s to $cwd, it must be a String"
                @@ Runtime.to_s next);
            post_block_hook :=
              Some
                (fun () ->
                  Runtime.string_of_val prev
                  |> option_value ~message:__LOC__
                  |> cd_wrapper)
        | _ -> ()
      in

      let _, either =
        List.fold assignments ~init:(inner_globals, First ())
          ~f:(fun prev (name, expr) ->
            prev >>= fun globals () ->
            interpret_expr globals expr >>= fun globals next_value ->
            let prev =
              match Context.get globals.context_ids name with
              | Some prev -> prev
              | None ->
                  Printf.sprintf "The context variable named %s is not defined"
                    name
                  |> fail ~globals pos
            in
            middleware name prev next_value;
            (match Context.reassign globals.context_ids name next_value with
            | Some () -> ()
            | None -> internal_failure __LOC__);
            (globals, First ()))
        >>= fun globals () -> (globals, interpret_block globals block)
      in
      (* This needs to run regardless of either's state *)
      let _ = Option.map !post_block_hook ~f:(fun hook -> hook ()) in
      (globals, either)
  | CatchExpr { subject; capture; catch; pos = _ } -> (
      let globals, subject_either = interpret_expr globals subject in
      match subject_either with
      | First _ as first -> (globals, first)
      | Second bt as second -> (
          match bt with
          | Error (_, msg) ->
              let inner_ids = Identifiers.push_empty globals.identifiers in
              (match Identifiers.bind inner_ids capture msg with
              | None -> failwith "TODO"
              | Some () -> ());
              let inner_globals = { globals with identifiers = inner_ids } in
              (* Don't let the inner_globals escape *)
              let _, either = interpret_expr inner_globals catch in
              (globals, either)
          | _ -> (globals, second)))

(** You must push an empty env frame on first *)
and interpret_block (globals : Globals.t) (stmts : Compiler.Optimizer.stmt list)
    : (Runtime.t, Runtime.breaking_type) Either.t =
  (* TODO can't use List.fold_left because we want to handle empty list
     differently *)
  let rec traverse_stmts globals' stmts =
    match stmts with
    | [] -> First Runtime.Null
    | hd :: tl -> (
        let globals'', either = interpret_stmt globals' hd in
        match either with
        | Second _ as second -> second
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
  match Hashtbl.find klass.instance_getters method_name with
  | None ->
      Printf.sprintf "The class %s does not have an instance field named %s"
        class_name method_name
      |> fail ~globals pos
  | Some thunk -> (
      match thunk receiver with
      | Ok t -> (
          match t with
          | Func func -> (
              match func with
              | User _ -> Sloth_common.Common.internal_failure __LOC__
              | Native { cb; _ } ->
                  let args = receiver :: args in
                  (globals, invoke_native_func ~globals ~pos cb args))
          | _ as t ->
              internal_failure
              @@ Printf.sprintf "Expected func, got %s (%s)" (Runtime.to_s t)
                   __LOC__)
      | Error msg -> (globals, Second (create_error ~globals ~pos msg)))

(* Actually call setter *)
and reassign_object_property ~globals ~pos receiver target newvalue =
  let class_name =
    match receiver with
    | Runtime.Prototype _ ->
        Printf.sprintf "TODO implement prototype property re-assignment (%s)"
          __LOC__
        |> failwith
    | _ -> Runtime.to_class_name receiver
  in
  let lookup =
    match Hashtbl.find Globals.(globals.classes) class_name with
    | None ->
        internal_failure
        @@ internal_failure_msg ~globals ~pos
        @@ Printf.sprintf
             "Internal error: could not find prototype for the %s class (%s)"
             class_name __LOC__
    | Some lookup -> lookup
  in
  match Hashtbl.find lookup.instance_setters target with
  | None ->
      Printf.sprintf "The class %s does not have a setter named %s" class_name
        target
      |> failure_obj ~globals ~pos
  | Some thunk -> (
      match thunk receiver newvalue with
      | Ok () -> Either.first Runtime.Null
      | Error msg -> failure_obj ~globals ~pos msg)

and dereference_object globals receiver target pos =
  let descriptor, class_name, table_thunk =
    let open Runtime in
    match receiver with
    (* Static access has different semantics *)
    | Prototype { name } -> ("a static", name, fun cl -> cl.static_getters)
    | _ ->
        ( "an instance",
          Runtime.to_class_name receiver,
          fun cl -> cl.instance_getters )
  in
  match Hashtbl.find globals.classes class_name with
  | None ->
      internal_failure
      @@ internal_failure_msg ~globals ~pos
      @@ Printf.sprintf
           "Internal error: could not find prototype for the %s class (%s)"
           class_name __LOC__
  | Some klass -> (
      match Hashtbl.find (table_thunk klass) target with
      | None ->
          Printf.sprintf "The class `%s` does not have %s field named \"%s\""
            class_name descriptor target
          |> failure_obj ~globals ~pos
      | Some thunk -> (
          match thunk receiver with
          | Ok t -> (
              match t with
              | Func func_t -> First (Method (receiver, func_t))
              | _ as other -> First other)
          | Error msg -> failure_obj ~globals ~pos msg))

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
    (Runtime.process, Runtime.breaking_type) Either.t =
  match v with
  | Runtime.Process p -> First p
  | Runtime.List _ as l -> (
      dereference_object globals
        (Runtime.Prototype { name = "Process" })
        "new" pos
      >>- fun constructor ->
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
        |> Dynarray.of_list
      in
      (cast_to_process [@tailcall]) ~globals ~pos (Runtime.List list)
  | _ as t' ->
      (* TODO: this should be a first class error *)
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
      interpret_expr globals rhs >>= fun globals rhs ->
      let either =
        cast_to_process ~globals ~pos lhs >>- fun left ->
        cast_to_process ~globals ~pos rhs >>- fun right ->
        let module M = (val globals.l) in
        let read, write = M.pipe () in
        left.stdout <- write;
        left.pipes_to_collect <- write :: left.pipes_to_collect;
        right.stdin <- read;
        right.pipes_to_collect <- read :: right.pipes_to_collect;
        let right = { right with previous = Some left } in
        Either.first @@ Runtime.Process right
      in
      (globals, either)
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
      if Float.(right = 0.0) then
        (globals, Either.second @@ create_error ~globals ~pos "Divide by zero")
      else (globals, Either.first @@ Runtime.Num (left /. right))
  | Product ->
      interpret_expr globals rhs >>= fun globals rhs ->
      let left = cast_to_number lhs in
      let right = cast_to_number rhs in
      (globals, Either.first @@ Runtime.Num (left *. right))
  | Modulo ->
      interpret_expr globals rhs >>= fun globals rhs ->
      let left = cast_to_number lhs in
      let right = cast_to_number rhs in
      let either =
        let left_either =
          if Float.is_integer left then Either.first @@ Float.to_int left
          else
            let msg =
              Printf.sprintf
                "Modulo (`%%`) can only operate on integers, got %f" left
            in
            failure_obj ~globals ~pos msg
        in

        let left_and_right_either =
          left_either >>- fun left ->
          if Float.is_integer right then Either.first (left, Float.to_int right)
          else
            let msg =
              Printf.sprintf
                "Modulo (`%%`) can only operate on integers, got %f" right
            in
            failure_obj ~globals ~pos msg
        in
        left_and_right_either >>- fun (left, right) ->
        Either.first @@ Runtime.Num (Float.of_int (left mod right))
      in
      (globals, either)
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
  | RightArrow -> interpret_right_arrow ~globals ~pos lhs rhs
  | Bang | AmpersandBang | Ampersand | Not | LeftArrow ->
      (* Not binary ops, unreachable *)
      Sloth_common.Common.internal_failure __LOC__

and interpret_right_arrow ~globals ~pos lhs rhs =
  interpret_expr globals rhs >>= fun globals rhs ->
  (* TODO O_CREAT? *)
  ( globals,
    cast_to_file_descriptor ~globals ~pos
      ~mode:[ Core_unix.O_WRONLY; Core_unix.O_CREAT ]
      ~m:globals.l rhs )
  >>= fun globals fd ->
  let module M = (val globals.l) in
  match lhs with
  | Process proc -> (
      proc.stdout <- fd;
      let env_val =
        Context.get globals.context_ids "$env" |> option_value ~message:__LOC__
      in
      let env =
        match Runtime.env_of_val env_val with
        | Some env -> env
        | None ->
            Printf.sprintf "$env is the wrong type: %s" @@ Runtime.to_s env_val
            |> fail ~globals pos
      in

      match M.proc_exec ~mode:BlockInherit proc env with
      | Ok _ -> (globals, First Runtime.Null)
      | Error msg -> (globals, failure_obj ~globals ~pos msg))
  | String txt -> (
      match M.fd_write_all fd txt with
      | Ok () -> (globals, First Runtime.Null)
      | Error msg -> (globals, failure_obj ~globals ~pos msg))
  | _ ->
      Printf.sprintf "TODO implement %s -> File" (Runtime.to_class_name lhs)
      |> failwith

and cast_to_file_descriptor ~globals ~pos ~mode ~m t :
    (Core_unix.File_descr.t, Runtime.breaking_type) Either.t =
  match t with
  | FileDescriptor fd -> First fd
  | File { path } -> (
      let module M = (val m : Native.Sig) in
      match M.open_file ~mode path with
      | Ok fd -> First fd
      | Error msg -> failure_obj ~globals ~pos msg)
  | String path ->
      let t = Runtime.File { path } in
      cast_to_file_descriptor ~globals ~pos ~mode ~m t
  | _ -> failure_obj ~globals ~pos "foo"
