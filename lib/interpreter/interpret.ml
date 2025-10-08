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
  let pos_s = Sloth_common.Position.string_of_t pos in
  let msg =
    Printf.sprintf "[%s] %s\n\n%s\n%s" pos_s type_s
      (Sloth_common.Position.summarize pos Globals.(globals.src))
      msg
  in
  if debug_mode then
    let callstack_depth = 50 in
    Printf.sprintf "%s\n\n%s" msg
      (* Core.Printexc does not implement .get_callstack *)
      (Stdlib.Printexc.get_callstack callstack_depth
      |> Stdlib.Printexc.raw_backtrace_to_string)
  else msg

let runtime_failure_msg ~globals ~pos msg =
  abstract_failure_msg ~globals ~pos msg "Runtime error"

let internal_failure_msg ~globals ~pos msg =
  abstract_failure_msg ~globals ~pos msg "Internal failure"

(* TODO make this a private type so we don't accidentally create error
   messages without source summaries *)
let failure_obj ~globals ~pos msg =
  let msg = runtime_failure_msg ~globals ~pos msg in
  Second (Runtime.create_error msg)

(** For error cases that could potentially become compiler errors.

    These should not be recoverable, you need to fix your code. *)
let fail ~globals pos msg =
  let msg = runtime_failure_msg ~globals ~pos msg in
  raise (RuntimeError msg)

let invoke_native_func ~globals ~pos cb args =
  let either =
    try cb Globals.(globals.context_ids) args
    with InternalFailure msg -> fail ~globals pos msg
  in
  match either with
  | First _ as first -> first
  | Second bt as second -> (
      match bt with
      | Runtime.Return _ | Break _ | Continue _ -> internal_failure __LOC__
      | Exit _ -> second
      | Error msg -> failure_obj ~globals ~pos (Runtime.to_s msg))

let rec interpret_prog globals prog =
  match prog with
  | [] -> (globals, First Runtime.Null)
  | hd :: tl -> (
      interpret_decl globals hd >>= fun new_globals v ->
      match tl with
      | [] -> (globals, First v) (* TODO is this right? *)
      | _ -> (interpret_prog [@tailcall]) new_globals tl)

and interpret_decl (globals : Globals.t) decl :
    Globals.t * (Runtime.t, Runtime.breaking_type) Either.t =
  let open Compiler.Optimizer in
  match decl with
  | FuncDecl { name; parameters; block; pos } ->
      let parameters = List.map parameters ~f:(fun (name, _) -> name) in
      let f =
        Runtime.Func
          (User { parameters; block; identifiers = globals.identifiers; pos })
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
  (* Globals.t * Compiler.Ast.breaking_type option * Runtime.t = *)
  let open Compiler.Optimizer in
  match stmt with
  | ExprStmt expr -> interpret_expr globals expr
  | BreakingStmt (break_type, expr_opt, _) -> (
      match expr_opt with
      | None -> (globals, First Runtime.Null)
      | Some e ->
          interpret_expr globals e >>= fun globals v ->
          let wrapped_type =
            match break_type with
            | Break -> Runtime.Break v
            | Continue -> Continue v
            | Return -> Return v
            | Error -> Error v
          in
          (globals, Second wrapped_type))

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
    Globals.t * (Runtime.t, Runtime.breaking_type) Either.t =
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
          | User { parameters; block; identifiers; pos = _ } ->
              let identifiers2 = Identifiers.push_empty identifiers in
              (* Bind args to env *)
              let or_unequal =
                List.fold2 parameters args ~init:(globals, First ())
                  ~f:(fun acc p a ->
                    acc >>= fun globals () ->
                    interpret_expr globals a >>= fun globals arg_val ->
                    (* This must not throw *)
                    Identifiers.bind identifiers2 p arg_val
                    |> option_value ~message:__LOC__;
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
          | Native { cb } ->
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
          | Native { cb } ->
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
                  cast_to_process ~globals ~pos arg
                  |> Either.map
                       ~first:(fun proc -> Runtime.Process proc)
                       ~second:Fun.id
                in
                (globals, either)
            | "String" -> (globals, First (Runtime.String (Runtime.to_s arg)))
            | _ -> fail ~globals pos (Printf.sprintf "unimplemented %s" name))
      | _ as t ->
          ( globals,
            failure_obj ~globals ~pos
              (Printf.sprintf "Tried to invoke %s, but it is not a function"
              @@ Runtime.to_s t) ))
  | FuncExpr { parameters; block; pos } ->
      let parameters = List.map parameters ~f:(fun (name, _) -> name) in
      let u =
        Runtime.User
          { parameters; block; identifiers = globals.identifiers; pos }
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
      | Ampersand ->
          interpret_expr globals target >>= fun globals target ->
          let either =
            cast_to_process ~globals ~pos target
            |> Either.map
                 ~first:(fun proc ->
                   let module M = (val globals.l : Native.Sig) in
                   (* TODO add error handling *)
                   let env =
                     Context.get globals.context_ids "$env"
                     |> option_value ~message:__LOC__
                     |> Runtime.env_of_val
                     |> option_value ~message:__LOC__
                   in
                   match M.proc_exec ~mode:Native.ForkBuffer proc env with
                   | Ok t' -> t'
                   | Error err -> fail ~globals pos err)
                 ~second:Fun.id
          in
          (globals, either)
      | AmpersandBang ->
          interpret_expr globals target >>= fun globals target ->
          let either =
            cast_to_process ~globals ~pos target
            |> Either.map
                 ~first:(fun proc ->
                   let module M = (val globals.l : Native.Sig) in
                   let env =
                     let t' =
                       Context.get globals.context_ids "$env"
                       |> option_value
                            ~message:"The context variable `$env` was not set!"
                     in
                     Runtime.env_of_val t'
                     |> option_value
                          ~message:
                            (Printf.sprintf
                               "Expected `$env` to be of type \
                                `HashMap[String]String`, but got %s"
                            @@ Runtime.to_s t')
                   in
                   match M.proc_exec ~mode:Native.BlockBuffer proc env with
                   | Ok t' -> t'
                   | Error err -> fail ~globals pos err)
                 ~second:Fun.id
          in
          (globals, either)
      | Bang ->
          interpret_expr globals target >>= fun globals target ->
          let either =
            cast_to_process ~globals ~pos target
            |> Either.map
                 ~first:(fun proc ->
                   let module M = (val globals.l : Native.Sig) in
                   let env =
                     let t' =
                       Context.get globals.context_ids "$env"
                       |> option_value
                            ~message:"The context variable `$env` was not set!"
                     in
                     Runtime.env_of_val t'
                     |> option_value
                          ~message:
                            (Printf.sprintf
                               "Expected `$env` to be of type \
                                `HashMap[String]String`, but got %s"
                            @@ Runtime.to_s t')
                   in
                   match M.proc_exec ~mode:Native.BlockInherit proc env with
                   | Ok t' -> t'
                   | Error err -> fail ~globals pos err)
                 ~second:Fun.id
          in
          (globals, either)
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
              |> option_value
                   ~message:(internal_failure_msg ~globals ~pos __LOC__);
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
                  Runtime.string_of_val prev
                  |> option_value ~message:__LOC__
                  |> M.chdir)
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
          | Error msg ->
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
      | Error msg -> (globals, Second (Runtime.create_error msg)))

and is_equal globals is_equality lhs rhs =
  let lh_s = Runtime.to_class_name lhs in
  let rh_s = Runtime.to_class_name rhs in
  let same_class = String.equal lh_s rh_s in
  (* if ==, then return false; if !=, then return true *)
  if not same_class then not is_equality
  else
    let ( >>= ) left right = if not left then left else right () in
    match lhs with
    | String lh_s ->
        let rh_s = Runtime.string_of_val rhs |> option_value ~message:__LOC__ in
        let same_string = String.equal lh_s rh_s in
        Bool.(same_string = is_equality)
    | Num lhs ->
        let rhs = Runtime.num_of_val rhs |> option_value ~message:__LOC__ in
        let same_float = Float.equal lhs rhs in
        Bool.(same_float = is_equality)
    | Bool lhs ->
        let rhs = Runtime.bool_of_val rhs |> option_value ~message:__LOC__ in
        let same_bool = Bool.( = ) lhs rhs in
        Bool.( = ) same_bool is_equality
    | List lhs ->
        let rhs = Runtime.list_of_val rhs |> option_value ~message:__LOC__ in
        let left_len = Array.length lhs in
        let right_len = Array.length rhs in
        let same_list =
          left_len = right_len >>= fun () ->
          Array.equal (is_equal globals true) lhs rhs
        in
        Bool.(same_list = is_equality)
    | HashMap lhs ->
        let rhs = Runtime.hashmap_of_val rhs |> option_value ~message:__LOC__ in
        let left_len = Stdlib.Hashtbl.length lhs in
        let right_len = Stdlib.Hashtbl.length rhs in
        let same_table =
          left_len = right_len >>= fun () ->
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
        Bool.(same_table = is_equality)
    | Null -> ( match rhs with Null -> is_equality | _ -> not is_equality)
    | Prototype { name = left_name } -> (
        match rhs with
        | Prototype { name = right_name } ->
            let names_same = String.(left_name = right_name) in
            Bool.(names_same = is_equality)
        | _ -> not is_equality)
    | Process lhs ->
        let rhs = Runtime.process_of_val rhs |> option_value ~message:__LOC__ in
        let rec inner_proc (lhs : Runtime.process) (rhs : Runtime.process) =
          let same_proc =
            (match
               List.fold2 lhs.cmd rhs.cmd ~init:true
                 ~f:(fun all_same left right ->
                   if all_same then String.(left = right) else false)
             with
            | Ok b -> b
            | Unequal_lengths -> false)
            >>= fun () ->
            Core_unix.File_descr.equal lhs.stdout rhs.stdout >>= fun () ->
            Core_unix.File_descr.equal lhs.stderr rhs.stderr >>= fun () ->
            Core_unix.File_descr.equal lhs.stdin rhs.stdin >>= fun () ->
            match
              List.fold2 lhs.pipes_to_collect rhs.pipes_to_collect ~init:true
                ~f:(fun all_same left right ->
                  if all_same then Core_unix.File_descr.equal left right
                  else false)
            with
            | Ok b -> b
            | Unequal_lengths ->
                false >>= fun () ->
                Option.equal
                  (fun proc1 proc2 -> inner_proc proc1 proc2)
                  lhs.previous rhs.previous
          in
          Bool.(same_proc = is_equality)
        in
        inner_proc lhs rhs
    | FileDescriptor lhs ->
        let rhs =
          Runtime.file_descriptor_of_t rhs |> option_value ~message:__LOC__
        in
        let same_fd = Core_unix.File_descr.equal lhs rhs in
        Bool.(same_fd = is_equality)
    | File { path = lhs } ->
        let rhs = Runtime.file_of_t rhs |> option_value ~message:__LOC__ in
        let same_file = String.(lhs = rhs.path) in
        Bool.(same_file = is_equality)
    | Directory lhs ->
        let rhs = Runtime.directory_of_t rhs |> option_value ~message:__LOC__ in
        let same_dir = String.(lhs = rhs) in
        Bool.(same_dir = is_equality)
    | ProcessHandle lhs ->
        let rhs =
          Runtime.process_handle_of_t rhs |> option_value ~message:__LOC__
        in
        let same_handle =
          match lhs with
          | ProcessInherited left_pid -> (
              match rhs with
              | ProcessInherited right_pid -> Pid.(left_pid = right_pid)
              | _ -> false)
          | ProcessBuffered
              { pid = left_pid; stdout = left_stdout; stderr = left_stderr }
            -> (
              match rhs with
              | ProcessBuffered
                  {
                    pid = right_pid;
                    stdout = right_stdout;
                    stderr = right_stderr;
                  } ->
                  Pid.(left_pid = right_pid) >>= fun () ->
                  Core_unix.File_descr.equal left_stdout right_stdout
                  >>= fun () ->
                  Core_unix.File_descr.equal left_stderr right_stderr
              | _ -> false)
        in
        Bool.(same_handle = is_equality)
    | Pipe (read, write) ->
        let r_read, r_write =
          Runtime.pipe_of_t rhs |> option_value ~message:__LOC__
        in
        let same_pipe =
          Core_unix.File_descr.(equal read r_read && equal write r_write)
        in
        Bool.(same_pipe = is_equality)
    | ProcessResult _ -> failwith "TODO"
    | Func _ | Method _ ->
        Printf.sprintf "is_equal the type %s is not implemented" lh_s
        |> failwith

and dereference_object globals receiver target pos =
  let descriptor, class_name, table_thunk =
    let open Runtime in
    match receiver with
    (* Static access has different semantics *)
    | Prototype { name } -> ("static", name, fun cl -> cl.static_getters)
    | _ ->
        ( "instance",
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
          Printf.sprintf "The class %s does not have a %s field named %s"
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
        ( globals,
          Either.second @@ Runtime.create_error
          @@ runtime_failure_msg ~globals ~pos "Divide by zero" )
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
    cast_to_file_descriptor ~globals ~pos ~mode:[ Core_unix.O_WRONLY ]
      ~m:globals.l rhs )
  >>= fun globals fd ->
  (match lhs with
  | Process proc -> proc.stdout <- fd
  | _ ->
      Printf.sprintf "TODO implement %s -> File" (Runtime.to_s lhs) |> failwith);
  (globals, Either.first lhs)

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
