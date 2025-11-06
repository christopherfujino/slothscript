open Core
open Sloth_common.Common

type proto_t = {
  name : string;
  getters : (string * (Runtime.t -> (Runtime.t, string) Result.t)) list;
  setters : (string * (Runtime.t -> Runtime.t -> (unit, string) Result.t)) list;
  static_getters : (string * (Runtime.t -> (Runtime.t, string) Result.t)) list;
}

let ( >>= ) left right =
  match left with Second _ as second -> second | First first -> right first

let make_native_func ~arity ~name cb =
  let open Runtime in
  let wrapped_cb ctx eval args =
    (match arity with
      | None -> Either.First ()
      | Some arity ->
          let arg_len = List.length args in
          (* TODO is this reachable now that we check in invoke_native_func? *)
          if not (Int.equal (List.length args) arity) then
            let msg =
              Printf.sprintf "You passed %d arguments (%s) but %d were expected"
                arg_len
                (List.fold_left args ~init:"" ~f:(fun msg arg ->
                     msg ^ Runtime.to_s arg ^ ", "))
                arity
            in
            Second (Runtime.Error (None, Runtime.String msg))
          else First ())
    >>= fun () -> cb ctx eval args
  in
  Func (Native { cb = wrapped_cb; name; arity })

let make_method ~arity ~name cb =
 fun self -> Ok (make_native_func ~arity ~name (cb self))

let make_ids m =
  let module M = (val m : Native.Sig) in
  [
    ( "print",
      make_native_func ~arity:(Some 1) ~name:"print" (fun ctx _ args ->
          match Context.get ctx "$stdout" with
          | None ->
              let err_msg = "The context variable `$stdout` was unset" in
              Second (Runtime.Error (None, Runtime.String err_msg))
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
              | Error msg -> Second (Runtime.Error (None, Runtime.String msg))))
    );
    ( "exit",
      make_native_func ~arity:(Some 1) ~name:"exit" (fun _ _ args ->
          let arg = List.hd_exn args in
          match Runtime.int_of_val arg with
          | Some code -> Second (Runtime.Exit code)
          | None ->
              let msg =
                Printf.sprintf
                  "The argument passed to `exit()` must be an integer, got %s"
                @@ Runtime.to_s arg
              in
              Second (Runtime.Error (None, Runtime.String msg))) );
    ( "assert",
      (* could be 1 or 2 *)
      make_native_func ~arity:None ~name:"assert" (fun _ _ args ->
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
                  | Some msg -> Ok (Printf.sprintf "Assertion failed: %s" msg))
              | None -> Ok "Assertion failed")
            |> Result.bind ~f:(fun err_msg ->
                match Runtime.bool_of_val arg with
                | Some condition ->
                    if condition then Ok (Runtime.Bool true) else Error err_msg
                | None ->
                    Error
                      (Printf.sprintf
                         "The first argument to assert() must be a Bool value, \
                          got %s"
                      @@ Runtime.to_s arg))
          in
          match res with
          | Ok v -> First v
          | Error err -> Second (Runtime.Error (None, Runtime.String err))) );
  ]

let make_protos m =
  let module M = (val m : Native.Sig) in
  [
    {
      name = "List";
      getters =
        [
          ( "contains",
            make_method ~arity:(Some 2) ~name:"List.contains"
              (fun self _ _ args ->
                let self =
                  Runtime.list_of_t self |> option_value ~message:__LOC__
                in
                let target = List.nth_exn args 1 in
                let b =
                  Dynarray.fold_left
                    (fun contains cur ->
                      if contains then true
                      else Runtime.is_equal true cur target)
                    false self
                in
                First (Runtime.Bool b)) );
          ( "filter",
            make_method ~arity:(Some 2) ~name:"List.filter"
              (fun self _ eval args ->
                let self =
                  Runtime.list_of_t self |> option_value ~message:__LOC__
                in
                let target = List.nth_exn args 1 in
                (match Runtime.func_of_val target with
                  | None ->
                      let msg =
                        Printf.sprintf
                          "Expected the first argument to `List.filter()` to \
                           be a function, but got %s"
                        @@ Runtime.to_s target
                      in
                      Second (Runtime.Error (None, Runtime.String msg))
                  | Some f -> First f)
                >>= fun f ->
                Dynarray.fold_left
                  (fun either el ->
                    either >>= fun new_arr ->
                    eval ~args:[ el ] f >>= fun b ->
                    (match Runtime.bool_of_val b with
                      | Some b -> First b
                      | None ->
                          let msg =
                            Printf.sprintf
                              "The return value of the callback passed to \
                               `List.filter()` should be `Bool`, but got %s"
                              (Runtime.to_s b)
                          in
                          Second Runtime.(Error (None, String msg)))
                    >>= fun b ->
                    First
                      (if b then (
                         Dynarray.add_last new_arr el;
                         new_arr)
                       else new_arr))
                  (First (Dynarray.create ()))
                  self
                >>= fun new_arr -> First (Runtime.List new_arr)) );
          ( "forEach",
            make_method ~arity:(Some 2) ~name:"List.forEach"
              (fun self _ eval args ->
                let self =
                  Runtime.list_of_t self |> option_value ~message:__LOC__
                in
                let target = List.nth_exn args 1 in
                (match Runtime.func_of_val target with
                  | None ->
                      let msg =
                        Printf.sprintf
                          "Expected the first argument to `List.forEach()` to \
                           be a function, but got %s"
                        @@ Runtime.to_s target
                      in
                      Second (Runtime.Error (None, Runtime.String msg))
                  | Some f -> First f)
                >>= fun f ->
                Dynarray.fold_left
                  (fun either el -> either >>= fun _ -> eval ~args:[ el ] f)
                  (First Runtime.Null) self
                >>= fun _ -> First Runtime.Null) );
          ( "length",
            fun self ->
              let arr_of_ts =
                Runtime.list_of_t self |> option_value ~message:__LOC__
              in
              let len = Dynarray.length arr_of_ts |> Float.of_int in
              Ok (Runtime.Num len) );
          ( "map",
            make_method ~arity:(Some 2) ~name:"List.map"
              (fun self _ eval args ->
                let self =
                  Runtime.list_of_t self |> option_value ~message:__LOC__
                in
                let target = List.nth_exn args 1 in
                (match Runtime.func_of_val target with
                  | None ->
                      let msg =
                        Printf.sprintf
                          "Expected the first argument to `List.map()` to be a \
                           function, but got %s"
                        @@ Runtime.to_s target
                      in
                      Second (Runtime.Error (None, Runtime.String msg))
                  | Some f -> First f)
                >>= fun f ->
                let new_arr = Dynarray.create () in
                Dynarray.fold_left
                  (fun either el ->
                    either >>= fun () ->
                    eval ~args:[ el ] f >>= fun v ->
                    First (Dynarray.add_last new_arr v))
                  (First ()) self
                >>= fun () -> First (Runtime.List new_arr)) );
          ( "pop",
            make_method ~arity:(Some 1) ~name:"List.pop" (fun self _ _ _ ->
                let self_arr =
                  Runtime.list_of_t self |> option_value ~message:__LOC__
                in
                if Dynarray.is_empty self_arr then First Runtime.Null
                else
                  let el = Dynarray.pop_last self_arr in
                  First el) );
          ( "push",
            make_method ~arity:(Some 2) ~name:"List.push" (fun self _ _ args ->
                let self_arr =
                  Runtime.list_of_t self |> option_value ~message:__LOC__
                in
                let arg = List.nth_exn args 1 in
                Dynarray.add_last self_arr arg;

                First Runtime.Null) );
          ( "reduce",
            make_method ~arity:(Some 2) ~name:"List.reduce"
              (fun self _ eval args ->
                let self =
                  Runtime.list_of_t self |> option_value ~message:__LOC__
                in
                let target = List.nth_exn args 1 in
                (match Runtime.func_of_val target with
                  | None ->
                      let msg =
                        Printf.sprintf
                          "Expected the first argument to `List.reduce()` to \
                           be a function, but got %s"
                        @@ Runtime.to_s target
                      in
                      Second (Runtime.Error (None, Runtime.String msg))
                  | Some f -> First f)
                >>= fun f ->
                match Dynarray.length self with
                | 0 -> First Runtime.Null
                | 1 -> First (Dynarray.get self 0)
                | _ ->
                    Dynarray.fold_left
                      (fun either cur ->
                        either >>= fun prev_opt ->
                        match prev_opt with
                        | None -> First (Some cur)
                        | Some prev ->
                            eval ~args:[ prev; cur ] f >>= fun v ->
                            First (Some v))
                      (* Use an option here so we know how to handle the first iteration *)
                      (First None) self
                    >>= fun opt -> First (option_value ~message:__LOC__ opt)) );
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
            make_method ~arity:(Some 1) ~name:"Pipe::new" (fun _ _ _ _ ->
                let read, write = M.pipe () in
                let t = Runtime.Pipe (read, write) in
                First t) );
        ];
    };
    {
      name = "Process";
      getters =
        [
          ( "forkBuffer",
            make_method ~arity:(Some 1) ~name:"Process.forkBuffer"
              (fun self ctx _ _ ->
                let self =
                  Runtime.process_of_t self |> option_value ~message:__LOC__
                in
                let env =
                  Context.get ctx "$env"
                  |> option_value
                       ~message:"The context variable `$env` was not set!"
                in
                let env =
                  Runtime.env_of_val env
                  |> option_value
                       ~message:
                         (Printf.sprintf
                            "Expected `$env` to be of type \
                             `HashMap[String]String`, but got %s"
                         @@ Runtime.to_s env)
                in
                match M.proc_exec ~mode:Native.ForkBuffer self env with
                | Ok t' -> First t'
                | Error err -> Second (Runtime.Error (None, String err))) );
          ( "blockBuffer",
            make_method ~arity:(Some 1) ~name:"Process.blockBuffer"
              (fun self ctx _ _ ->
                let self =
                  Runtime.process_of_t self |> option_value ~message:__LOC__
                in
                let env =
                  Context.get ctx "$env"
                  |> option_value
                       ~message:"The context variable `$env` was not set!"
                in
                let env =
                  Runtime.env_of_val env
                  |> option_value
                       ~message:
                         (Printf.sprintf
                            "Expected `$env` to be of type \
                             `HashMap[String]String`, but got %s"
                         @@ Runtime.to_s env)
                in
                match M.proc_exec ~mode:Native.BlockBuffer self env with
                | Ok t' -> First t'
                | Error err -> Second (Runtime.Error (None, String err))) );
          ( "blockInherit",
            make_method ~arity:(Some 1) ~name:"Process.blockInherit"
              (fun self ctx _ _ ->
                let self =
                  Runtime.process_of_t self |> option_value ~message:__LOC__
                in
                let env =
                  Context.get ctx "$env"
                  |> option_value
                       ~message:"The context variable `$env` was not set!"
                in
                let env =
                  Runtime.env_of_val env
                  |> option_value
                       ~message:
                         (Printf.sprintf
                            "Expected `$env` to be of type \
                             `HashMap[String]String`, but got %s"
                         @@ Runtime.to_s env)
                in
                match M.proc_exec ~mode:Native.BlockInherit self env with
                | Ok t' -> First t'
                | Error err -> Second (Runtime.Error (None, String err))) );
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
            make_method ~arity:(Some 2) ~name:"Process::new"
              (fun _ ctx _ args ->
                let arg = List.nth_exn args 1 in
                match Runtime.list_of_t arg with
                | None ->
                    let err_msg =
                      Printf.sprintf
                        "Expected the first argument to `Process::new` to be a \
                         List[String] but got `%s`"
                      @@ Runtime.to_s arg
                    in
                    Second (Runtime.Error (None, Runtime.String err_msg))
                | Some arr ->
                    (* Not efficient *)
                    Dynarray.fold_right
                      (fun t acc ->
                        acc >>= fun prev ->
                        let string_opt = Runtime.string_of_val t in
                        match string_opt with
                        | Some s -> First (s :: prev)
                        | None ->
                            let msg =
                              Printf.sprintf
                                "Expected the first argument to `Process.new` \
                                 to be a List[String], but got a non-String \
                                 element"
                            in
                            Second (Runtime.Error (None, Runtime.String msg)))
                      arr (First [])
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
            make_method ~arity:(Some 1) ~name:"ProcessHandle.wait"
              (fun handle _ _ _ ->
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
                | Error msg -> Second (Runtime.Error (None, Runtime.String msg)))
          );
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
            make_method ~arity:(Some 2) ~name:"HashMap.merge"
              (fun left _ _ args ->
                let left =
                  left |> Runtime.hashmap_of_val
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
                      let bt = Runtime.Error (None, Runtime.String msg) in
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
          ( "contains",
            make_method ~arity:(Some 2) ~name:"String.contains"
              (fun self _ _ args ->
                let self_string =
                  Runtime.string_of_val self |> option_value ~message:__LOC__
                in
                let first_arg = List.nth_exn args 1 in
                let err_msg =
                  Printf.sprintf
                    "Expected the first argument to String::contains() to be a \
                     `String`, but got %s"
                    (Runtime.to_s first_arg)
                in
                let pattern =
                  Runtime.string_of_val first_arg
                  |> option_value ~message:err_msg
                in
                let b =
                  match String.substr_index ~pos:0 self_string ~pattern with
                  | Some _ -> true
                  | None -> false
                in
                First (Runtime.Bool b)) );
          ( "trim",
            make_method ~arity:(Some 1) ~name:"String.trim" (fun self _ _ _ ->
                let ocaml_string =
                  Runtime.string_of_val self |> option_value ~message:__LOC__
                in
                First (Runtime.String (String.strip ocaml_string))) );
          ( "split",
            make_method ~arity:(Some 2) ~name:"String.split"
              (fun self _ _ args ->
                let first_arg = List.nth_exn args 1 in
                let self_string =
                  Runtime.string_of_val self |> option_value ~message:__LOC__
                in
                match Runtime.string_of_val first_arg with
                | None ->
                    let msg =
                      Printf.sprintf
                        "Expected first arg to be a `String` but got %s"
                        (Runtime.to_s first_arg)
                    in
                    Second (Error (None, Runtime.String msg))
                | Some sep -> (
                    let sep_len = String.length sep in
                    match sep_len with
                    | 1 ->
                        let ch = sep.[0] in
                        let str_arr =
                          String.split self_string ~on:ch
                          |> List.map ~f:(fun s -> Runtime.String s)
                          |> Dynarray.of_list
                        in
                        First (Runtime.List str_arr)
                    | _ ->
                        let indices =
                          String.substr_index_all self_string ~pattern:sep
                            ~may_overlap:false
                        in
                        let last, reversed_parts =
                          List.fold indices ~init:(0, [])
                            ~f:(fun (prev, parts) index ->
                              let len = index - prev in
                              let part =
                                String.sub self_string ~pos:prev ~len
                              in
                              (index + sep_len, Runtime.String part :: parts))
                        in
                        let last_string =
                          String.sub self_string ~pos:last
                            ~len:(String.length self_string - last)
                        in
                        let reversed_parts =
                          Runtime.String last_string :: reversed_parts
                        in
                        let parts_arr =
                          List.rev reversed_parts |> Dynarray.of_list
                        in
                        First (Runtime.List parts_arr))) );
        ];
      setters = [];
      static_getters = [];
    };
    {
      name = "Directory";
      getters =
        [
          ( "exists",
            make_method ~arity:(Some 1) ~name:"Directory.exists"
              (fun self _ _ _ ->
                let path =
                  Runtime.directory_of_t self |> option_value ~message:__LOC__
                in
                let b = M.directory_exists path in
                First (Runtime.Bool b)) );
          ( "create",
            make_method ~arity:(Some 1) ~name:"Directory.create"
              (fun self _ _ _ ->
                let path =
                  Runtime.directory_of_t self |> option_value ~message:__LOC__
                in
                M.mkdir path;
                First Runtime.Null) );
          ( "path",
            fun self ->
              let path =
                Runtime.directory_of_t self |> option_value ~message:__LOC__
              in
              Ok (Runtime.String path) );
        ];
      setters = [];
      static_getters = [];
    };
    {
      name = "File";
      getters =
        [
          ( "exists",
            make_method ~arity:(Some 1) ~name:"File.exists" (fun self _ _ _ ->
                let Runtime.{ path } =
                  Runtime.file_of_t self |> option_value ~message:__LOC__
                in
                let b = M.file_exists path in
                First (Runtime.Bool b)) );
          ( "openRead",
            make_method ~arity:(Some 1) ~name:"File.openRead" (fun self _ _ _ ->
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
                | Error msg -> Second (Error (None, Runtime.String msg))) );
          ( "openWrite",
            make_method ~arity:(Some 1) ~name:"File.openWrite"
              (fun self _ _ _ ->
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
                | Error msg -> Second (Error (None, Runtime.String msg))) );
          ( "path",
            fun self ->
              let Runtime.{ path } =
                Runtime.file_of_t self |> option_value ~message:__LOC__
              in
              Ok (Runtime.String path) );
          ( "readString",
            make_method ~arity:(Some 1) ~name:"File.readString"
              (fun self _ _ _ ->
                let Runtime.{ path } =
                  match Runtime.file_of_t self with
                  | Some f -> f
                  | None ->
                      internal_failure
                      @@ Printf.sprintf "Expected a file, but got %s (%s)"
                           (Runtime.to_s self) __LOC__
                in
                (* Errors? *)
                let contents = M.file_read_all path in
                First (Runtime.String contents)) );
        ];
      setters = [];
      static_getters =
        [
          ( "new",
            make_method ~arity:(Some 2) ~name:"File::new" (fun self _ _ _ ->
                (match Runtime.string_of_val self with
                  | None ->
                      Second
                        (Runtime.Error
                           ( None,
                             Runtime.String
                               (Printf.sprintf
                                  "You must pass a String to File::new(), but \
                                   got a %s"
                               @@ Runtime.to_s self) ))
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
            make_method ~arity:(Some 1) ~name:"FileDescriptor.close"
              (fun fd_t _ _ _ ->
                let fd =
                  match Runtime.file_descriptor_of_t fd_t with
                  | Some fd -> fd
                  | None -> Sloth_common.Common.internal_failure __LOC__
                in
                match M.close fd with
                | Ok () -> First Runtime.Null
                | Error msg -> Second (Error (None, String msg))) );
          ( "read",
            make_method ~arity:(Some 1) ~name:"FileDescriptor.read"
              (fun fd_t _ _ _ ->
                let fd =
                  match Runtime.file_descriptor_of_t fd_t with
                  | Some fd -> fd
                  | None -> Sloth_common.Common.internal_failure __LOC__
                in
                match M.read fd with
                | Ok contents -> First contents
                | Error msg -> Second (Error (None, String msg))) );
          ( "readAll",
            make_method ~arity:(Some 1) ~name:"FileDescriptor.readAll"
              (fun fd_t _ _ _ ->
                let fd =
                  match Runtime.file_descriptor_of_t fd_t with
                  | Some fd -> fd
                  | None -> Sloth_common.Common.internal_failure __LOC__
                in
                match M.fd_read_all fd with
                | Ok contents -> First contents
                | Error msg -> Second (Error (None, String msg))) );
          ( "write",
            make_method ~arity:(Some 2) ~name:"FileDescriptor.write"
              (fun fd_t _ _ args ->
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
                           `FileDescriptor.write(s)` should be a `String`, but \
                           got %s"
                        @@ Runtime.to_s contents
                      in
                      let runtime_t = Runtime.String msg in
                      Second (Runtime.Error (None, runtime_t)))
                >>= fun str ->
                match M.write fd ~data:str with
                | Ok () -> First Runtime.Null
                | Error msg -> Second (Error (None, String msg))) );
          ( "writeAll",
            make_method ~arity:(Some 2) ~name:"FileDescriptor.writeAll"
              (fun fd_t _ _ args ->
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
                           `FileDescriptor.writeAll(s)` should be a `String`, \
                           but got %s"
                        @@ Runtime.to_s contents
                      in
                      let runtime_t = Runtime.String msg in
                      Second (Runtime.Error (None, runtime_t)))
                >>= fun str ->
                match M.fd_write_all fd str with
                | Ok () -> First Runtime.Null
                | Error msg -> Second (Error (None, String msg))) );
        ];
      setters = [];
      static_getters = [];
    };
  ]

let context_ids ~cwd ~env ~script_path ~argv =
  [
    ( "$argv",
      Runtime.List
        (Dynarray.of_list @@ List.map argv ~f:(fun s -> Runtime.String s)) );
    ("$cwd", Runtime.String cwd);
    ("$env", Runtime.val_of_env env);
    ("$script", Runtime.String script_path);
    ("$scriptDir", Runtime.String (Filename.dirname script_path));
    ("$stderr", Runtime.FileDescriptor Core_unix.stderr);
    ("$stdin", Runtime.FileDescriptor Core_unix.stdin);
    ("$stdout", Runtime.FileDescriptor Core_unix.stdout);
  ]
