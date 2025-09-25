open Core

type t = {
  l : (module Native.Sig);
  identifiers : Runtime.t Identifiers.t;
  context_ids : Context.t;
  (* We need to store these at the top level so that we can find these for
   runtime lookup. They must also be stored in identifiers however, so users
   can invoke them explicitly. *)
  classes : Runtime.class_lookup;
  src : string;
  script_path : string;
}

let make_globals m src script_path ~env =
  let module M = (val m : Native.Sig) in
  let classes = Hashtbl.create (module String) in
  let identifiers = Identifiers.create () in
  let context_ids = Context.create () in
  let make_method name arity methods
      (cb :
        Runtime.t list ->
        (Runtime.t, Compiler.Ast.breaking_type * Runtime.t) Either.t) =
    let cb :
        Runtime.t list ->
        (Runtime.t, Compiler.Ast.breaking_type * Runtime.t) Either.t =
     fun args ->
      let arg_len = List.length args in
      if not (Int.equal arg_len arity) then
        Second
          ( Compiler.Ast.Error
              (Printf.sprintf
                 "You passed %d arguments (%s) to %s but %d were expected"
                 arg_len
                 (List.fold_left args ~init:"" ~f:(fun msg arg ->
                      msg ^ Runtime.to_s arg ^ ", "))
                 name arity),
            Runtime.Null )
      else cb args
    in
    let native =
      Runtime.Native
        { parameters = [ "this"; "left"; "right" ]; cb; identifiers }
    in
    Hashtbl.add_exn methods ~key:name ~data:(Runtime.Func native)
  in
  let make_func name ?arity identifiers cb =
    let (cb
          : Runtime.t list ->
            (Runtime.t, Compiler.Ast.breaking_type * Runtime.t) Either.t) =
     fun args ->
      let arg_len = List.length args in
      if
        Option.is_some arity && not (Int.equal arg_len (Option.value_exn arity))
      then
        Second
          ( Compiler.Ast.Error
              (Printf.sprintf "You passed %d arguments but %d were expected"
                 arg_len (Option.value_exn arity)),
            Runtime.Null )
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
              First Runtime.Null)
      | "exit" ->
          make_func "exit" ~arity:1 identifiers (fun args ->
            let arg = List.hd_exn args in
            match Runtime.int_of_val arg with
            | Some code -> Second ((Compiler.Ast.Exit code), Runtime.Null)
            | None ->
                let msg = Printf.sprintf "The argument passed to `exit()` must be an integer, got %s" @@ Runtime.to_s arg in
                Second ((Compiler.Ast.Error msg), Runtime.Null)
          )
      | "assert" ->
          make_func "assert" identifiers (fun args ->
              let arg = List.hd_exn args in
              let second_arg = List.nth args 1 in
              let res =
                (match second_arg with
                | Some msg -> (
                    match Runtime.string_of_val msg with
                    | None ->
                        let msg =
                          Printf.sprintf
                            "The second argument to assert() must be a String, \
                             got %s"
                          @@ Runtime.to_s msg
                        in
                        Error msg
                    | Some msg -> Ok (Printf.sprintf "Assertion failed: %s" msg)
                    )
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
                             @@ Runtime.to_s arg))
              in
              match res with
              | Ok v -> First v
              | Error err -> Second (Error err, Runtime.Null))
      | _ ->
          Printf.sprintf "TODO: You have not yet implemented %s at %s" name
            __LOC__
          |> Sloth_common.Common.internal_failure);
  List.iter Sloth_common.Stdlib_interface.globals.context_ids ~f:(function
    | "$cwd" ->
        (* TODO does this need to be injected? *)
        let cwd = Sys_unix.getcwd () in
        Context.bind context_ids "$cwd" (Runtime.String cwd) |> Option.value_exn
    | "$env" ->
        Context.bind context_ids "$env" (Runtime.val_of_env env)
        |> Option.value_exn
    | "$script" ->
        Context.bind context_ids "$script" (Runtime.String script_path)
        |> Option.value_exn
    | "$scriptDir" ->
        let script_dir = Filename.dirname script_path in
        Context.bind context_ids "$scriptDir" (Runtime.String script_dir)
        |> Option.value_exn
    | _ as name ->
        Printf.sprintf
          "TODO: You have not yet implemented the context var %s at %s" name
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
                  make_method name 2 cl.static_members (fun args ->
                      let arg = List.nth_exn args 1 in
                      match Runtime.list_of_val arg with
                      | None ->
                          let err_msg =
                            Printf.sprintf
                              "Expected the first argument to `Process.new` to \
                               be a List[String] but got `%s`"
                            @@ Runtime.to_s arg
                          in
                          Second (Compiler.Ast.Error err_msg, Runtime.Null)
                      | Some arr ->
                          let cmd_either =
                            List.of_array arr
                            (* Not efficient *)
                            |> List.fold_right ~init:(First []) ~f:(fun t acc ->
                                   match acc with
                                   | First prev -> (
                                       let string_opt =
                                         Runtime.string_of_val t
                                       in
                                       match string_opt with
                                       | Some s -> First (s :: prev)
                                       | None ->
                                           Second
                                             ( Compiler.Ast.Error
                                                 (Printf.sprintf
                                                    "Expected the first \
                                                     argument to `Process.new` \
                                                     to be a List[String], but \
                                                     got a non-String element"),
                                               Runtime.Null ))
                                   | Second _ as sec -> sec)
                          in
                          Either.map cmd_either ~second:Fun.id
                            ~first:(fun cmd ->
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
      | "ProcessResult" ->
          List.iter methods ~f:(fun meth ->
              match meth with
              | "stdout" ->
                  make_method meth 1 cl.instance_members (fun args ->
                      let proc = List.hd_exn args in
                      let result =
                        Runtime.process_result_of_val proc |> Option.value_exn
                      in
                      First (Runtime.String result.stdout))
              | _ ->
                  Printf.sprintf
                    "`ProcessResult` does not implement the method `%s`" meth
                  |> failwith);
          List.iter static_members ~f:(fun name ->
              match name with
              | _ ->
                  Printf.sprintf
                    "`ProcessResult` does not implement the static member `%s`"
                    name
                  |> failwith)
      | "HashMap" ->
          List.iter methods ~f:(fun meth ->
              match meth with
              | "merge" ->
                  make_method meth 2 cl.instance_members (fun args ->
                      let left =
                        List.hd_exn args |> Runtime.hashmap_of_val
                        |> Option.value_exn
                      in
                      let right_v = List.nth_exn args 1 in
                      let right =
                        match Runtime.hashmap_of_val right_v with
                        | Some right -> right
                        | None ->
                            Printf.sprintf
                              "HashMap.merge() expects an argument of type \
                               `HashMap`, but received %s"
                              (Runtime.to_s right_v)
                            |> failwith
                      in
                      let left_copy = Stdlib.Hashtbl.copy left in
                      Stdlib.Hashtbl.iter
                        (fun key v -> Stdlib.Hashtbl.add left_copy key v)
                        right;
                      First (Runtime.HashMap left_copy))
              | _ ->
                  Printf.sprintf "`HashMap` does not implement the method `%s`"
                    meth
                  |> failwith);
          List.iter static_members ~f:(fun name ->
              match name with
              | _ ->
                  Printf.sprintf
                    "`HashMap` does not implement the static member `%s`" name
                  |> failwith)
      | "String" ->
          List.iter methods ~f:(fun meth ->
              match meth with
              | "trim" ->
                  make_method meth 1 cl.instance_members (fun args ->
                      let str = List.hd_exn args in
                      let ocaml_string =
                        Runtime.string_of_val str |> Option.value_exn
                      in
                      First (Runtime.String (String.strip ocaml_string)))
              | _ ->
                  Printf.sprintf "`String` does not implement the method `%s`"
                    meth
                  |> failwith);
          List.iter static_members ~f:(fun name ->
              match name with
              | _ ->
                  Printf.sprintf
                    "`String` does not implement the static member `%s`" name
                  |> failwith)
      | "Directory" ->
          List.iter methods ~f:(fun meth ->
              match meth with
              | "exists" ->
                  make_method meth 1 cl.instance_members (fun args ->
                      let path =
                        List.hd_exn args |> Runtime.directory_of_val
                        |> Option.value_exn
                      in
                      let b = M.directory_exists path in
                      First (Runtime.Bool b))
              | "create" ->
                  make_method meth 1 cl.instance_members (fun args ->
                      let path =
                        List.hd_exn args |> Runtime.directory_of_val
                        |> Option.value_exn
                      in
                      M.mkdir path;
                      First Runtime.Null)
              | "path" ->
                  make_method meth 1 cl.instance_members (fun args ->
                      let path =
                        List.hd_exn args |> Runtime.directory_of_val
                        |> Option.value_exn
                      in
                      First (Runtime.String path))
              | _ ->
                  Printf.sprintf "`Directory does not implement the method `%s`"
                    meth
                  |> failwith);
          List.iter static_members ~f:(fun name ->
              match name with
              | _ ->
                  Printf.sprintf
                    "`Directory` does not implement the static member `%s`" name
                  |> failwith)
      | "File" ->
          List.iter methods ~f:(fun meth ->
              match meth with
              | "readString" ->
                  make_method meth 2 cl.instance_members (fun args ->
                      let first_arg = List.nth_exn args 1 in
                      let Runtime.{ path } =
                        match Runtime.file_of_val first_arg with
                        | Some f -> f
                        | None -> Sloth_common.Common.internal_failure __LOC__
                      in
                      (* Errors? *)
                      let contents = M.file_read_all path in
                      First (Runtime.String contents))
              | _ ->
                  Printf.sprintf "`File` does not implement the method `%s`"
                    meth
                  |> failwith);
          List.iter static_members ~f:(fun name ->
              match name with
              | "new" ->
                  make_method name 2 cl.static_members (fun args ->
                      let first_arg = List.nth_exn args 1 in
                      (match Runtime.string_of_val first_arg with
                      | None ->
                          Second
                            ( Compiler.Ast.Error
                                (Printf.sprintf
                                   "You must pass a String to File.new(), but \
                                    got a %s"
                                @@ Runtime.to_s first_arg),
                              Runtime.Null )
                      | Some str -> First str)
                      |> Either.map ~second:Fun.id ~first:(fun path ->
                             Runtime.File { path }))
              | _ ->
                  Printf.sprintf
                    "`File` does not implement the static member `%s`" name
                  |> failwith)
      | _ ->
          Printf.sprintf
            "TODO: class named \"%s\" has not been implemented in %s" name
            __FILE__
          |> failwith);
      (match Hashtbl.add classes ~key:name ~data:cl with
      | `Duplicate ->
          Printf.sprintf "Tried to implement the class %s twice" name
          |> failwith
      | `Ok -> ());
      (* Bind at the root scope so users can reach it from IdRefs *)
      Identifiers.bind identifiers name (Runtime.Prototype { name })
      |> Option.value_exn);

  { l = m; identifiers; src; classes; context_ids; script_path }
