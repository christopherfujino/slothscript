open Core

type t = {
  l : (module Sloth_stdlib.StdlibSig);
  identifiers : Runtime.t Identifiers.t;
  (* We need to store these at the top level so that we can find these for
   runtime lookup. They must also be stored in identifiers however, so users
   can invoke them explicitly. *)
  classes : Runtime.class_lookup;
  src : string;
}

let make_ctx m src =
  let module M = (val m : Sloth_stdlib.StdlibSig) in
  let classes = Hashtbl.create (module String) in
  let identifiers = Identifiers.create () in
  let make_method name arity methods cb =
    Hashtbl.add_exn methods ~key:name
      ~data:
        (Runtime.Func
           (Runtime.Native
              {
                parameters = [ "this"; "left"; "right" ];
                cb =
                  (fun args ->
                    let arg_len = List.length args in
                    if not (Int.equal arg_len arity) then
                      Error
                        (Printf.sprintf
                           "You passed %d arguments (%s) but %d were expected"
                           arg_len
                           (List.fold_left args ~init:"" ~f:(fun msg arg ->
                                msg ^ Runtime.to_s arg ^ ", "))
                           arity)
                    else cb args);
                identifiers;
              }))
  in
  let make_func name ?arity identifiers cb =
    Identifiers.bind identifiers name
      (Runtime.Func
         (Native
            {
              parameters = [ "value" ];
              cb =
                (fun args ->
                  let arg_len = List.length args in
                  if
                    Option.is_some arity
                    && not (Int.equal arg_len (Option.value_exn arity))
                  then
                    Error
                      (Printf.sprintf
                         "You passed %d arguments but %d were expected" arg_len
                         (Option.value_exn arity))
                  else cb args);
              identifiers;
            }))
    |> Option.value_exn
  in
  List.iter Sloth_common.Stdlib_interface.globals.ids ~f:(fun name ->
      match name with
      | "print" ->
          make_func "print" ~arity:1 identifiers (fun args ->
              let arg = List.hd_exn args in
              Runtime.to_s arg |> M.print_s;
              M.print_s "\n";
              Ok Runtime.Null)
      | "assert" ->
          make_func "assert" identifiers (fun args ->
              let arg = List.hd_exn args in
              let second_arg = List.nth args 1 in
              (match second_arg with
              | Some msg -> (
                  match Runtime.string_of_val msg with
                  | Some msg -> Ok (Printf.sprintf "Assertion failed: %s" msg)
                  | None ->
                      Error
                        (Printf.sprintf
                           "The second argument to assert() must be a String, \
                            got %s"
                        @@ Runtime.to_s msg))
              | None -> Ok "Assertion failed")
              |> Result.bind ~f:(fun err_msg ->
                     match Runtime.bool_of_val arg with
                     | Some condition ->
                         if condition then Ok (Runtime.Bool true)
                         else Error err_msg
                     | None ->
                         Error
                           (Printf.sprintf
                              "The first argument to assert() must be a Bool \
                               value, got %s"
                           @@ Runtime.to_s arg)))
      | "$cwd" ->
          Identifiers.bind identifiers "$cwd" (Runtime.String "TODO")
          |> Option.value_exn
      | _ ->
          Printf.sprintf "TODO: You have not yet implemented %s" name
          |> failwith);
  List.iter Sloth_common.Stdlib_interface.globals.protos
    ~f:(fun { name; methods; static_members } ->
      let cl =
        Runtime.
          {
            instance_members = Hashtbl.create (module String);
            static_members = Hashtbl.create (module String);
          }
      in
      (match name with
      | "Number" ->
          List.iter methods ~f:(fun meth ->
              let process_infix_methods args cb =
                let lhs =
                  List.nth_exn args 0 |> Runtime.num_of_val |> Option.value_exn
                in
                let rhs_t = List.nth_exn args 1 in
                match Runtime.num_of_val rhs_t with
                | None ->
                    Error
                      (Printf.sprintf
                         "Expected right-hand side to be a Number, but got %s"
                      @@ Runtime.to_s rhs_t)
                | Some rhs -> Ok (cb lhs rhs)
              in
              match meth with
              | "+" ->
                  make_method "+" 2 cl.instance_members (fun args ->
                      process_infix_methods args (fun lhs rhs ->
                          Runtime.Num (lhs +. rhs)))
              | "-" ->
                  make_method "-" 2 cl.instance_members (fun args ->
                      process_infix_methods args (fun lhs rhs ->
                          Runtime.Num (lhs -. rhs)))
              | "/" ->
                  make_method "/" 2 cl.instance_members (fun args ->
                      process_infix_methods args (fun lhs rhs ->
                          Runtime.Num (lhs /. rhs)))
              | "*" ->
                  make_method "*" 2 cl.instance_members (fun args ->
                      process_infix_methods args (fun lhs rhs ->
                          Runtime.Num (lhs *. rhs)))
              | "<=" ->
                  make_method "<=" 2 cl.instance_members (fun args ->
                      process_infix_methods args (fun lhs rhs ->
                          Runtime.Bool Float.(lhs <=. rhs)))
              | ">=" ->
                  make_method ">=" 2 cl.instance_members (fun args ->
                      process_infix_methods args (fun lhs rhs ->
                          Runtime.Bool Float.(lhs >=. rhs)))
              | ">" ->
                  make_method ">" 2 cl.instance_members (fun args ->
                      process_infix_methods args (fun lhs rhs ->
                          Runtime.Bool Float.(lhs >. rhs)))
              | "<" ->
                  make_method "<" 2 cl.instance_members (fun args ->
                      process_infix_methods args (fun lhs rhs ->
                          Runtime.Bool Float.(lhs <. rhs)))
              | _ ->
                  failwith
                    (Printf.sprintf "Number does not implement the method %s"
                       meth));
          List.iter static_members ~f:(fun _ -> failwith __LOC__)
      | "Process" ->
          List.iter methods ~f:(fun meth ->
              let process_infix_methods args cb =
                let lhs =
                  List.nth_exn args 0 |> Runtime.num_of_val |> Option.value_exn
                in
                let rhs_t = List.nth_exn args 1 in
                let err_cb () =
                  Printf.sprintf
                    "Expected right-hand side to be a Number, but got %s"
                  @@ Runtime.to_s rhs_t
                  |> failwith
                in
                let rhs = Runtime.process_of_val ~cb:err_cb rhs_t in
                Ok (cb lhs rhs)
              in
              match meth with
              | "|" ->
                  make_method meth 2 cl.instance_members (fun args ->
                      process_infix_methods args (fun _ _ -> failwith __LOC__))
              | "!" ->
                  make_method meth 1 cl.instance_members (fun args ->
                      let process = List.hd_exn args in
                      let cmd =
                        match process with
                        | Process { cmd } -> cmd
                        | _ ->
                            Printf.sprintf "Internal error %s\n\n%s"
                              (Runtime.to_class_name process)
                              __LOC__
                            |> failwith
                      in

                      let prog = List.hd_exn cmd in

                      let pid = Core_unix.fork_exec ~prog ~argv:cmd () in
                      match Core_unix.waitpid pid with
                      | Ok () -> Ok (Runtime.ProcessResult { code = 0 })
                      | Error _ ->
                          Error
                            "Your subprocess failed with a mysterious(?) error")
              | _ ->
                  Printf.sprintf "`Process` does not implement the method `%s`"
                    meth
                  |> failwith);
          List.iter static_members ~f:(fun name ->
              match name with
              | "new" ->
                  make_method name 1 cl.static_members (fun args ->
                      let arg = List.hd_exn args in
                      match Runtime.list_of_val arg with
                      | None ->
                          Error
                            (Printf.sprintf
                               "Expected the first argument to `Process.new` \
                                to be a List[String] but got `%s`"
                            @@ Runtime.to_s arg)
                      | Some arr ->
                          let cmd_res =
                            List.of_array arr
                            (* Not efficient *)
                            |> List.fold_right ~init:(Ok []) ~f:(fun t acc ->
                                   match acc with
                                   | Ok prev -> (
                                       let string_opt =
                                         Runtime.string_of_val t
                                       in
                                       match string_opt with
                                       | Some s -> Ok (s :: prev)
                                       | None ->
                                           Error
                                             (Printf.sprintf
                                                "Expected the first argument \
                                                 to `Process.new` to be a \
                                                 List[String], but got a \
                                                 non-String element"))
                                   | Error _ -> acc)
                          in
                          Result.map cmd_res ~f:(fun cmd ->
                              Runtime.Process { cmd }))
              | _ ->
                  Printf.sprintf
                    "`Process` does not implement the static member `%s`" name
                  |> failwith)
      | _ ->
          Printf.sprintf
            "TODO: class named %s has not been implemented in context.ml" name
          |> failwith);
      (match Hashtbl.add classes ~key:name ~data:cl with
      | `Duplicate ->
          Printf.sprintf "Tried to implement the class %s twice" name
          |> failwith
      | `Ok -> ());
      Identifiers.bind identifiers name (Runtime.Prototype { name })
      |> Option.value_exn);

  { l = m; identifiers; src; classes }
