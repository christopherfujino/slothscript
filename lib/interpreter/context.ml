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

let exec_proc proc =
  let rec get_pids proc =
    let prev_pids =
      match Runtime.(proc.previous) with
      | Some prev -> get_pids prev
      | None -> []
    in
    let prog = List.hd_exn Runtime.(proc.cmd) in

    let this_pid =
      match Core_unix.fork () with
      | `In_the_child ->
          Core_unix.dup2 ~src:proc.stdin ~dst:Core_unix.stdin ();
          Core_unix.dup2 ~src:proc.stdout ~dst:Core_unix.stdout ();
          let _ = Core_unix.exec ~prog ~argv:proc.cmd () in
          failwith "unreachable"
      | `In_the_parent pid ->
          List.iter proc.pipes_to_collect ~f:(fun pipe -> Core_unix.close pipe);
          pid
    in
    this_pid :: prev_pids
  in

  let pids = get_pids proc in

  (* First is the last in the queue *)
  let last_pid = List.hd_exn pids in
  match Core_unix.waitpid last_pid with
  | Error _ -> Error "Your subprocess failed with a mysterious(?) error"
  | Ok () ->
      (* TODO check all the other pids too *)
      Ok (Runtime.ProcessResult { code = 0 })

let make_ctx m src =
  let module M = (val m : Sloth_stdlib.StdlibSig) in
  let classes = Hashtbl.create (module String) in
  let identifiers = Identifiers.create () in
  let make_method name arity methods cb =
    let cb =
     fun args ->
      let arg_len = List.length args in
      if not (Int.equal arg_len arity) then
        Error
          (Printf.sprintf "You passed %d arguments (%s) but %d were expected"
             arg_len
             (List.fold_left args ~init:"" ~f:(fun msg arg ->
                  msg ^ Runtime.to_s arg ^ ", "))
             arity)
      else cb args
    in
    let native =
      Runtime.Native
        { parameters = [ "this"; "left"; "right" ]; cb; identifiers }
    in
    Hashtbl.add_exn methods ~key:name ~data:(Runtime.Func native)
  in
  let make_func name ?arity identifiers cb =
    let cb =
     fun args ->
      let arg_len = List.length args in
      if
        Option.is_some arity && not (Int.equal arg_len (Option.value_exn arity))
      then
        Error
          (Printf.sprintf "You passed %d arguments but %d were expected" arg_len
             (Option.value_exn arity))
      else cb args
    in
    Identifiers.bind identifiers name
      (Runtime.Func (Native { parameters = [ "value" ]; cb; identifiers }))
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
          Printf.sprintf "TODO: You have not yet implemented %s at %s" name
            __LOC__
          |> Sloth_common.Common.internal_failure);
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
              match meth with
              | _ ->
                  failwith
                    (Printf.sprintf "Number does not implement the method %s"
                       meth));
          List.iter static_members ~f:(fun _ -> failwith __LOC__)
      | "Process" ->
          List.iter methods ~f:(fun meth ->
              match meth with
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
                              let proc =
                                Runtime.
                                  {
                                    cmd;
                                    stdin = Core_unix.stdin;
                                    stdout = Core_unix.stdout;
                                    stderr = Core_unix.stderr;
                                    previous = None;
                                    pipes_to_collect = [];
                                  }
                              in
                              Runtime.Process proc))
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
      (* Bind at the root scope so users can reach it from IdRefs *)
      Identifiers.bind identifiers name (Runtime.Prototype { name })
      |> Option.value_exn);

  { l = m; identifiers; src; classes }
