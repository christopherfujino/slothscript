open Core
open Sloth_common.Common

let ( >>= ) left right =
  match left with Second _ as second -> second | First first -> right first

let make_func ~arity cb =
  let open Runtime in
  let wrapped_cb =
   fun ctx args ->
    (match arity with
    | None -> Either.First ()
    | Some arity ->
        let arg_len = List.length args in
        if not (Int.equal (List.length args) arity) then
          Second
            (Runtime.create_error
               (Printf.sprintf
                  "You passed %d arguments (%s) but %d were expected" arg_len
                  (List.fold_left args ~init:"" ~f:(fun msg arg ->
                       msg ^ Runtime.to_s arg ^ ", "))
                  arity))
        else First ())
    >>= fun () -> cb ctx args
  in
  Func (Native { cb = wrapped_cb })

let make_method ~arity cb = fun _ -> Ok (make_func ~arity cb)

let make_ids m =
  let module M = (val m : Native.Sig) in
  [
    ( "print",
      make_func ~arity:(Some 1) (fun ctx args ->
          match Context.get ctx "$stdout" with
          | None ->
              let err_msg = "The context variable `$stdout` was unset" in
              Second (Runtime.create_error err_msg)
          | Some stdout -> (
              let open Result.Monad_infix in
              let res =
                (match Runtime.file_descriptor_of_t stdout with
                | Some s -> Ok s
                | None ->
                    Error
                      (Printf.sprintf
                         "Expected `$stdout` to be of type `FileDescriptor`, \
                          but instead it was %s"
                         (Runtime.to_s stdout)))
                >>= fun stdout ->
                let arg = List.hd_exn args in
                let data = Runtime.to_s arg ^ "\n" in
                M.write stdout ~data
              in

              match res with
              | Ok () -> First Runtime.Null
              | Error msg -> Second (Runtime.create_error msg))) );
    ( "exit",
      make_func ~arity:(Some 1) (fun _ args ->
          let arg = List.hd_exn args in
          match Runtime.int_of_val arg with
          | Some code -> Second (Runtime.Exit code)
          | None ->
              let msg =
                Printf.sprintf
                  "The argument passed to `exit()` must be an integer, got %s"
                @@ Runtime.to_s arg
              in
              Second (Runtime.create_error msg)) );
    ( "assert",
      (* could be 1 or 2 *)
      make_func ~arity:None (fun _ args ->
          let arg = List.hd_exn args in
          let second_arg = List.nth args 1 in
          let res =
            (match second_arg with
            | Some msg -> (
                match Runtime.string_of_val msg with
                | None ->
                    let msg =
                      Printf.sprintf
                        "The second argument to assert() must be a String, got \
                         %s"
                      @@ Runtime.to_s msg
                    in
                    Error msg
                | Some msg -> Ok (Printf.sprintf "Assertion failed: %s" msg))
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
          | Error err -> Second (Runtime.create_error err)) );
  ]

type proto_t = {
  name : string;
  getters : (string * (Runtime.t -> (Runtime.t, string) Result.t)) list;
  setters : (string * (Runtime.t -> Runtime.t -> (unit, string) Result.t)) list;
  static_getters : (string * (Runtime.t -> (Runtime.t, string) Result.t)) list;
}

let make_protos m =
  let module M = (val m : Native.Sig) in
  [
    {
      name = "List";
      getters =
        [
          ( "length",
            make_method ~arity:(Some 1) (fun _ args ->
                let arg = List.hd_exn args in
                let arr_of_ts =
                  Runtime.list_of_val arg |> option_value ~message:__LOC__
                in
                let len = Array.length arr_of_ts |> Float.of_int in
                First (Runtime.Num len)) );
        ];
      setters = [];
      static_getters = [];
    };
    { name = "Number"; getters = []; setters = []; static_getters = [] };
    {
      name = "Pipe";
      getters =
        [
          ( "read",
            fun self ->
              let read, _ =
                Runtime.pipe_of_t self |> Option.value_exn ~message:__LOC__
              in
              Ok (FileDescriptor read) );
          ( "write",
            fun self ->
              let _, write =
                Runtime.pipe_of_t self |> Option.value_exn ~message:__LOC__
              in
              Ok (FileDescriptor write) );
        ];
      setters = [];
      static_getters =
        [
          ( "new",
            make_method ~arity:(Some 1) (fun _ _ ->
                let read, write = M.pipe () in
                let t = Runtime.Pipe (read, write) in
                First t) );
        ];
    };
    {
      name = "Process";
      getters =
        [
          ( "stdout",
            fun self ->
              let proc =
                Runtime.process_of_t self |> option_value ~message:__LOC__
              in
              Ok (FileDescriptor proc.stdout) );
          ( "stderr",
            fun self ->
              let proc =
                Runtime.process_of_t self |> option_value ~message:__LOC__
              in
              Ok (FileDescriptor proc.stderr) );
        ];
      setters =
        [
          ( "stdout",
            fun self next ->
              let proc =
                Runtime.process_of_t self |> option_value ~message:__LOC__
              in
              match next with
              | FileDescriptor fd ->
                  proc.stdout <- fd;
                  Ok ()
              | _ ->
                  Error
                    (Printf.sprintf "Expected a `FileDescriptor` but got %s"
                    @@ Runtime.to_s next) );
          ( "stderr",
            fun self next ->
              let proc =
                Runtime.process_of_t self |> option_value ~message:__LOC__
              in
              match next with
              | FileDescriptor fd ->
                  proc.stderr <- fd;
                  Ok ()
              | _ ->
                  Error
                    (Printf.sprintf "Expected a `FileDescriptor` but got %s"
                    @@ Runtime.to_s next) );
        ];
      static_getters =
        [
          ( "new",
            make_method ~arity:(Some 2) (fun ctx args ->
                let arg = List.nth_exn args 1 in
                match Runtime.list_of_val arg with
                | None ->
                    let err_msg =
                      Printf.sprintf
                        "Expected the first argument to `Process.new` to be a \
                         List[String] but got `%s`"
                      @@ Runtime.to_s arg
                    in
                    Second (Runtime.create_error err_msg)
                | Some arr ->
                    List.of_array arr (* Not efficient *)
                    |> List.fold_right ~init:(First []) ~f:(fun t acc ->
                           match acc with
                           | First prev -> (
                               let string_opt = Runtime.string_of_val t in
                               match string_opt with
                               | Some s -> First (s :: prev)
                               | None ->
                                   Second
                                     (Runtime.create_error
                                     @@ Printf.sprintf
                                          "Expected the first argument to \
                                           `Process.new` to be a List[String], \
                                           but got a non-String element"))
                           | Second _ as sec -> sec)
                    >>= fun cmd ->
                    let unwrap_fd identifier =
                      let t' =
                        Context.get ctx identifier
                        |> option_value
                             ~message:
                               (Printf.sprintf
                                  "The context variable `%s` has been unset"
                                  identifier)
                      in
                      Runtime.file_descriptor_of_t t'
                      |> option_value
                           ~message:
                             (Printf.sprintf
                                "Expected `%s` to be of type `FileDescriptor`, \
                                 but instead it was %s"
                                identifier
                             @@ Runtime.to_s t')
                    in
                    let stdin = unwrap_fd "$stdin" in
                    let stdout = unwrap_fd "$stdout" in
                    let stderr = unwrap_fd "$stderr" in
                    let proc =
                      Runtime.
                        {
                          cmd;
                          stdin;
                          stdout;
                          stderr;
                          previous = None;
                          pipes_to_collect = [];
                        }
                    in
                    First (Runtime.Process proc)) );
        ];
    };
    {
      name = "ProcessHandle";
      getters =
        [
          ( "wait",
            make_method ~arity:(Some 1) (fun _ args ->
                let handle = List.hd_exn args in
                let handle =
                  Runtime.process_handle_of_t handle
                  |> option_value ~message:__LOC__
                in
                let res =
                  match handle with
                  | ProcessInherited _ ->
                      Sloth_common.Common.internal_failure __LOC__
                  | ProcessBuffered _ -> M.wait handle
                in
                match res with
                | Ok proc_result -> First proc_result
                | Error msg -> Second (Runtime.create_error msg)) );
        ];
      setters = [];
      static_getters = [];
    };
    {
      name = "ProcessResult";
      getters =
        [
          ( "stderr",
            fun self ->
              let Runtime.{ code = _; stdout = _; stderr } =
                Runtime.process_result_of_val self |> Option.value_exn
              in
              Ok (Runtime.String stderr) );
          ( "stdout",
            fun self ->
              let result =
                Runtime.process_result_of_val self
                |> option_value ~message:__LOC__
              in
              Ok (Runtime.String result.stdout) );
        ];
      setters = [];
      static_getters = [];
    };
    {
      name = "HashMap";
      getters =
        [
          ( "merge",
            make_method ~arity:(Some 2) (fun _ args ->
                let left =
                  List.hd_exn args |> Runtime.hashmap_of_val
                  |> option_value ~message:__LOC__
                in
                let right_v = List.nth_exn args 1 in
                (match Runtime.hashmap_of_val right_v with
                | Some right -> First right
                | None ->
                    let msg =
                      Printf.sprintf
                        "HashMap.merge() expects an argument of type \
                         `HashMap`, but received %s"
                        (Runtime.to_s right_v)
                    in
                    let bt = Runtime.create_error msg in
                    Second bt)
                >>= fun right ->
                let left_copy = Stdlib.Hashtbl.copy left in
                Stdlib.Hashtbl.iter
                  (fun key v -> Stdlib.Hashtbl.add left_copy key v)
                  right;
                First (Runtime.HashMap left_copy)) );
        ];
      setters = [];
      static_getters = [];
    };
    {
      name = "String";
      getters =
        [
          ( "trim",
            make_method ~arity:(Some 1) (fun _ args ->
                let str = List.hd_exn args in
                let ocaml_string =
                  Runtime.string_of_val str |> option_value ~message:__LOC__
                in
                First (Runtime.String (String.strip ocaml_string))) );
        ];
      setters = [];
      static_getters = [];
    };
    {
      name = "Directory";
      getters =
        [
          ( "exists",
            make_method ~arity:(Some 1) (fun _ args ->
                let path =
                  List.hd_exn args |> Runtime.directory_of_t
                  |> option_value ~message:__LOC__
                in
                let b = M.directory_exists path in
                First (Runtime.Bool b)) );
          ( "create",
            make_method ~arity:(Some 1) (fun _ args ->
                let path =
                  List.hd_exn args |> Runtime.directory_of_t
                  |> option_value ~message:__LOC__
                in
                M.mkdir path;
                First Runtime.Null) );
          ( "path",
            make_method ~arity:(Some 1) (fun _ args ->
                let path =
                  List.hd_exn args |> Runtime.directory_of_t
                  |> option_value ~message:__LOC__
                in
                First (Runtime.String path)) );
        ];
      setters = [];
      static_getters = [];
    };
    {
      name = "File";
      getters =
        [
          ( "openRead",
            make_method ~arity:(Some 1) (fun _ args ->
                let self = List.hd_exn args in
                let Runtime.{ path } =
                  match Runtime.file_of_t self with
                  | Some p -> p
                  | None ->
                      internal_failure
                      @@ Printf.sprintf "Expected a file, but got %s (%s)"
                           (Runtime.to_s self) __LOC__
                in
                match M.open_file ~mode:[ Core_unix.O_RDONLY ] path with
                | Ok fd -> First (Runtime.FileDescriptor fd)
                | Error msg -> Second (Error (Runtime.String msg))) );
          ( "openWrite",
            make_method ~arity:(Some 1) (fun _ args ->
                let self = List.hd_exn args in
                let Runtime.{ path } =
                  match Runtime.file_of_t self with
                  | Some p -> p
                  | None ->
                      internal_failure
                      @@ Printf.sprintf "Expected a file, but got %s (%s)"
                           (Runtime.to_s self) __LOC__
                in
                match
                  M.open_file
                    ~mode:[ Core_unix.O_WRONLY; Core_unix.O_CREAT ]
                    path
                with
                | Ok fd -> First (Runtime.FileDescriptor fd)
                | Error msg -> Second (Error (Runtime.String msg))) );
          ( "readString",
            make_method ~arity:(Some 1) (fun _ args ->
                let first_arg = List.hd_exn args in
                let Runtime.{ path } =
                  match Runtime.file_of_t first_arg with
                  | Some f -> f
                  | None ->
                      internal_failure
                      @@ Printf.sprintf "Expected a file, but got %s (%s)"
                           (Runtime.to_s first_arg) __LOC__
                in
                (* Errors? *)
                let contents = M.file_read_all path in
                First (Runtime.String contents)) );
        ];
      setters = [];
      static_getters =
        [
          ( "new",
            make_method ~arity:(Some 2) (fun _ args ->
                let first_arg = List.nth_exn args 1 in
                (match Runtime.string_of_val first_arg with
                | None ->
                    Second
                      (Runtime.create_error
                      @@ Printf.sprintf
                           "You must pass a String to File.new(), but got a %s"
                      @@ Runtime.to_s first_arg)
                | Some str -> First str)
                |> Either.map ~second:Fun.id ~first:(fun path ->
                       Runtime.File { path })) );
        ];
    };
    {
      name = "FileDescriptor";
      getters =
        [
          ( "close",
            make_method ~arity:(Some 1) (fun _ args ->
                let fd_t = List.hd_exn args in
                let fd =
                  match Runtime.file_descriptor_of_t fd_t with
                  | Some fd -> fd
                  | None -> Sloth_common.Common.internal_failure __LOC__
                in
                match M.close fd with
                | Ok () -> First Runtime.Null
                | Error msg -> Second (Error (String msg))) );
          ( "read",
            make_method ~arity:(Some 1) (fun _ args ->
                let fd_t = List.hd_exn args in
                let fd =
                  match Runtime.file_descriptor_of_t fd_t with
                  | Some fd -> fd
                  | None -> Sloth_common.Common.internal_failure __LOC__
                in
                match M.read fd with
                | Ok contents -> First contents
                | Error msg -> Second (Error (String msg))) );
          ( "readAll",
            make_method ~arity:(Some 1) (fun _ args ->
                let fd_t = List.hd_exn args in
                let fd =
                  match Runtime.file_descriptor_of_t fd_t with
                  | Some fd -> fd
                  | None -> Sloth_common.Common.internal_failure __LOC__
                in
                match M.fd_read_all fd with
                | Ok contents -> First contents
                | Error msg -> Second (Error (String msg))) );
          ( "writeAll",
            make_method ~arity:(Some 2) (fun _ args ->
                let fd_t = List.hd_exn args in
                let fd =
                  match Runtime.file_descriptor_of_t fd_t with
                  | Some fd -> fd
                  | None -> Sloth_common.Common.internal_failure __LOC__
                in
                let contents = List.nth_exn args 1 in
                (match Runtime.string_of_val contents with
                | Some s -> First s
                | None ->
                    let msg =
                      Printf.sprintf
                        "The first argument passed to \
                         `FileDescriptor.writeAll()` should be a `String`, but \
                         got %s"
                      @@ Runtime.to_s contents
                    in
                    let runtime_t = Runtime.String msg in
                    Second (Runtime.Error runtime_t))
                >>= fun str ->
                match M.fd_write_all fd str with
                | Ok () -> First Runtime.Null
                | Error msg -> Second (Error (String msg))) );
        ];
      setters = [];
      static_getters = [];
    };
  ]

let context_ids ~cwd ~env ~script_path ~argv =
  [
    ( "$argv",
      Runtime.List
        (List.to_array @@ List.map argv ~f:(fun s -> Runtime.String s)) );
    ("$cwd", Runtime.String cwd);
    ("$env", Runtime.val_of_env env);
    ("$script", Runtime.String script_path);
    ("$scriptDir", Runtime.String (Filename.dirname script_path));
    ("$stderr", Runtime.FileDescriptor Core_unix.stderr);
    ("$stdin", Runtime.FileDescriptor Core_unix.stdin);
    ("$stdout", Runtime.FileDescriptor Core_unix.stdout);
  ]
