open Core
open Common

let failure ~ctx pos msg =
  let pos_s = Sloth_common.Position.string_of_t pos in
  let msg1 =
    Printf.sprintf "%s\n\n[%s] Runtime error: %s"
      (Sloth_common.Position.summarize pos Context.(ctx.src))
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
      (match Identifiers.bind ctx.identifiers name f with
      | Some () -> ()
      | None ->
          Printf.sprintf "A function named %s has already been declared" name
          |> failure ~ctx pos);
      (ctx, f)
  | StmtDecl s -> interpret_stmt ctx s

and interpret_stmt (ctx : Context.t) stmt =
  let open Compiler.Optimizer in
  match stmt with
  | LetStmt (id, e, pos) ->
      let v = interpret_expr ctx e in
      (match Identifiers.bind ctx.identifiers id v with
      | Some () -> ()
      | None ->
          Printf.sprintf
            "The name %s has already been declared in this scope; did you mean \
             to assign to it?"
            id
          |> failure ~ctx pos);
      (ctx, v)
  | AssignStmt (id, e, pos) ->
      let v = interpret_expr ctx e in
      (match Identifiers.reassign ctx.identifiers id v with
      | Some () -> ()
      | None ->
          Printf.sprintf
            "The name %s has not been declared yet; did you mean to declare it?"
            id
          |> failure ~ctx pos);
      (ctx, v)
  | SubAssignStmt { subscript; value; pos } -> (
      match subscript with
      | Subscript (receiver, subscript, pos) -> (
          let receiver' = interpret_expr ctx receiver in
          let subscript' = interpret_expr ctx subscript in
          let value' = interpret_expr ctx value in
          match receiver' with
          | HashMap tbl ->
              Stdlib.Hashtbl.replace tbl subscript' value';
              (ctx, value')
          | List elements -> (
              match Runtime.int_of_val subscript' with
              | Some i ->
                  Array.set elements i value';
                  (ctx, receiver')
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
      | _ -> Printf.sprintf "Unreachable?! %s" __LOC__ |> failure ~ctx pos)
  | ExprStmt expr -> (ctx, interpret_expr ctx expr)
  | ForLoop (init, cmp, inc, bl, pos) ->
      let identifiers = Identifiers.push_empty ctx.identifiers in
      let ctx' = { ctx with identifiers } in
      let ctx'', _ = interpret_stmt ctx' init in

      let rec interpret_for_loop ctx cmp inc bl last_val =
        let cmp_val = interpret_expr ctx cmp in
        match cmp_val |> Runtime.bool_of_val with
        | Some cmp_val ->
            if not cmp_val then last_val
            else
              (* Each iteration should have its own scope *)
              let inner_ctx =
                Context.
                  {
                    ctx with
                    identifiers = Identifiers.push_empty ctx.identifiers;
                  }
              in

              let cur_val = interpret_block inner_ctx bl in
              let ctx, _ = interpret_stmt ctx inc in
              (interpret_for_loop [@tailcall]) ctx cmp inc bl cur_val
        | None ->
            Printf.sprintf
              "The comparison of a for loop must be a Boolean value, but you \
               used %s"
              (Runtime.to_s cmp_val)
            |> failure ~ctx pos
      in
      (ctx, interpret_for_loop ctx'' cmp inc bl Runtime.Null)
  | ForInLoop { iterator_name; iteratee; block; pos } ->
      let ctx =
        { ctx with identifiers = Identifiers.push_empty ctx.identifiers }
      in
      let iteratee = interpret_expr ctx iteratee in
      let iteratee_array =
        match iteratee with
        | List l -> l
        | _ ->
            Printf.sprintf "Cannot iterate over a %s"
              (Runtime.to_class_name iteratee)
            |> failure ~ctx pos
      in
      let return_value =
        Array.fold iteratee_array ~init:Runtime.Null ~f:(fun _ element ->
            let ctx =
              { ctx with identifiers = Identifiers.push_empty ctx.identifiers }
            in
            Identifiers.bind ctx.identifiers iterator_name element
            |> Option.value_exn;
            interpret_block ctx block)
      in
      (ctx, return_value)

and interpret_cond ctx cond =
  match cond with
  | Compiler.Optimizer.IfCont { conditional; block; continuation; pos } -> (
      let condition = interpret_expr ctx conditional in
      match Runtime.bool_of_val condition with
      | Some condition_b -> (
          if condition_b then
            let ctx =
              { ctx with identifiers = Identifiers.push_empty ctx.identifiers }
            in
            interpret_block ctx block
          else
            match continuation with
            | None -> Runtime.Null
            | Some cond -> (interpret_cond [@tailcall]) ctx cond)
      | None ->
          Printf.sprintf
            "If-expressions must have a boolean expression, but you used %s"
            (Runtime.to_s condition)
          |> failure ~ctx pos)
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
                Printf.sprintf
                  "Lists can only be subscripted by integers, you used %s"
                  (Runtime.to_s subscript')
                |> failure ~ctx pos
          | _ ->
              failure ~ctx pos
                (Runtime.to_s subscript'
                |> Printf.sprintf
                     "Lists can only be subscripted by Numbers, you used %s"))
      | Runtime.HashMap tbl -> Stdlib.Hashtbl.find tbl subscript'
      | _ ->
          Printf.sprintf "Cannot subscript the value %s"
            (Runtime.to_s receiver')
          |> failure ~ctx pos)
  | IdRef (i, pos) -> (
      match Identifiers.get ctx.identifiers i with
      | Some v -> v
      | None ->
          Printf.sprintf "The name %s has not been declared in this scope" i
          |> failure ~ctx pos)
  | Equality (lhs, rhs, is_equality, _) ->
      let lhs = interpret_expr ctx lhs in
      let rhs = interpret_expr ctx rhs in

      Runtime.Bool (is_equal ctx is_equality lhs rhs)
  | Binary (lhs, rhs, op, pos) -> interpret_binary ctx lhs rhs op pos
  | MethodInvoc { receiver; target; args; pos } ->
      interpret_method ~ctx ~pos receiver args target
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
                     Identifiers.bind identifiers2 param_name v
                     (* This must not throw *)
                     |> Option.value_exn)
               with
              | Ok () -> ()
              | Unequal_lengths ->
                  Printf.sprintf
                    "Mismatched number of params and args in call to function"
                  |> failure ~ctx pos);
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
          | Native { cb; parameters = _; identifiers = _ } -> (
              let arg_vals =
                List.map args ~f:(fun arg -> interpret_expr ctx arg)
              in
              match cb arg_vals with
              | Ok v -> v
              | Error msg -> failure ~ctx pos msg))
      | Prototype { name } -> (
          if not @@ phys_equal (List.length args) 1 then failwith "TODO"
          else
            let arg_expr = List.hd_exn args in
            let arg = interpret_expr ctx arg_expr in
            match name with
            | "File" -> Runtime.File (cast_to_file ~ctx ~pos arg)
            | _ -> Sloth_common.Common.internal_failure __LOC__)
      | _ ->
          Printf.sprintf "Tried to invoke %s, but it is not a function"
            (Runtime.to_s receiver')
          |> failwith)
  | FuncExpr { parameters; block; _ } ->
      let parameters = List.map parameters ~f:(fun (name, _) -> name) in
      Func (User { parameters; block; identifiers = ctx.identifiers })
  | IfExpr (cond, _) -> interpret_cond ctx cond
  | UnaryExpr { target; pos; operator } -> (
      (* TODO deprecate is_prefix when we've removed prefix bang *)
      let v = interpret_expr ctx target in
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
          | Some b -> Runtime.Bool (not b))
      | Bang -> (
          let target = interpret_expr ctx target in
          let proc = cast_to_process ~ctx ~pos target in
          match Context.exec_proc proc with
          | Ok t' -> t'
          | Error err -> failure ~ctx pos err)
      | LeftArrow -> (
          let target = interpret_expr ctx target in
          let file = cast_to_file ~ctx ~pos target in
          let target = Runtime.File file in
          let func = dereference_object ctx target "readString" pos in
          let func =
            match Runtime.func_of_val func with
            | None -> Sloth_common.Common.internal_failure __LOC__
            | Some func -> func
          in
          let cb =
            match func with
            | User _ -> Sloth_common.Common.internal_failure __LOC__
            | Native { cb; _ } -> cb
          in
          match cb [ target ] with
          | Error err -> failure ~ctx pos err
          | Ok t' -> t')
      | Plus | Minus | Product | Divide | Pipe | Less | Greater | Leq | Geq
      | RightArrow ->
          (* Unreachable *) Sloth_common.Common.internal_failure __LOC__)
  | DoBlock (block, _) ->
      let ctx =
        { ctx with identifiers = Identifiers.push_empty ctx.identifiers }
      in
      interpret_block ctx block
  | ObjDeref (receiver, target, pos) ->
      let receiver = interpret_expr ctx receiver in
      dereference_object ctx receiver target pos

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

and interpret_method ~ctx ~pos receiver args method_name =
  let receiver = interpret_expr ctx receiver in
  let args = List.map args ~f:(interpret_expr ctx) in
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
          | User _ -> Printf.sprintf "Internal error: %s" __LOC__ |> failwith
          | Native { cb; _ } -> (
              let args = receiver :: args in
              match cb args with Ok v -> v | Error msg -> failure ~ctx pos msg))
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
      | Some func -> func)

and cast_to_file ~ctx ~pos = function
  | Runtime.File f -> f
  | Runtime.String filename -> (
      let func_opt =
        dereference_object ctx (Runtime.Prototype { name = "File" }) "new" pos
        |> Runtime.func_of_val
      in
      let constructor =
        match func_opt with
        | None -> Sloth_common.Common.internal_failure __LOC__
        | Some func -> func
      in
      let callback =
        match constructor with
        | Native { cb; _ } -> cb
        | User _ -> Sloth_common.Common.internal_failure __LOC__
      in
      match callback [ Runtime.String filename ] with
      | Ok file -> (cast_to_file [@tailcall]) ~ctx ~pos file
      | Error err -> failure ~ctx pos err)
  | _ as t' ->
      failure ~ctx pos
      @@ Printf.sprintf "Expected a File but got a %s"
      @@ Runtime.to_s t'

and cast_to_process ~ctx ~pos = function
  | Runtime.Process p -> p
  | Runtime.List _ as l ->
      let constructor_opt =
        dereference_object ctx
          (Runtime.Prototype { name = "Process" })
          "new" pos
        |> Runtime.func_of_val
      in
      let constructor =
        match constructor_opt with
        | None -> Sloth_common.Common.internal_failure __LOC__
        | Some cons -> cons
      in
      let callback =
        match constructor with
        | Native { cb; _ } -> cb
        | User _ -> Sloth_common.Common.internal_failure __LOC__
      in
      let proc =
        match callback [ l ] with
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
  let lhs = interpret_expr ctx lhs in
  let rhs = interpret_expr ctx rhs in
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
      Runtime.Process right
  | Plus ->
      let left = cast_to_number lhs in
      let right = cast_to_number rhs in
      Runtime.Num (left +. right)
  | Minus ->
      let left = cast_to_number lhs in
      let right = cast_to_number rhs in
      Runtime.Num (left -. right)
  | Divide ->
      let left = cast_to_number lhs in
      let right = cast_to_number rhs in
      Runtime.Num (left /. right)
  | Product ->
      let left = cast_to_number lhs in
      let right = cast_to_number rhs in
      Runtime.Num (left *. right)
  | Leq ->
      let left = cast_to_number lhs in
      let right = cast_to_number rhs in
      Runtime.Bool Float.(left <= right)
  | Geq ->
      let left = cast_to_number lhs in
      let right = cast_to_number rhs in
      Runtime.Bool Float.(left >= right)
  | Less ->
      let left = cast_to_number lhs in
      let right = cast_to_number rhs in
      Runtime.Bool Float.(left < right)
  | Greater ->
      let left = cast_to_number lhs in
      let right = cast_to_number rhs in
      Runtime.Bool Float.(left > right)
  | RightArrow ->
      let left = cast_to_string ~ctx ~pos lhs in
      let Runtime.{ path } = cast_to_file ~ctx ~pos rhs in
      Out_channel.write_all path ~data:left;
      Runtime.String left
  | Bang | Not | LeftArrow ->
      (* Not binary ops, unreachable *)
      Sloth_common.Common.internal_failure __LOC__

and cast_to_string ~ctx ~pos t' =
  let open Runtime in
  match t' with
  | String s -> s
  | ProcessResult { stdout; _ } -> stdout
  | Process proc -> (
      match Context.exec_proc proc with
      | Ok t' -> (cast_to_string [@tailcall]) ~ctx ~pos t'
      | Error err -> failure ~ctx pos err)
  | _ as t' ->
      failure ~ctx pos
      @@ Printf.sprintf "Expected a String, but got a %s"
      @@ Runtime.to_s t'
