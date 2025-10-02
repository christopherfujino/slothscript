open Core

type func_t = {
  name : string;
  arity : int option;
  cb :
    Runtime.t list ->
    (Runtime.t, Compiler.Ast.breaking_type * Runtime.t) Either.t;
}

let make_ids m =
  let module M = (val m : Native.Sig) in
  [
    {
      name = "print";
      arity = Some 1;
      cb =
        (fun args ->
          let arg = List.hd_exn args in
          Runtime.to_s arg |> M.print_s;
          M.print_s "\n";
          First Runtime.Null);
    };
    {
      name = "exit";
      arity = Some 1;
      cb =
        (fun args ->
          let arg = List.hd_exn args in
          match Runtime.int_of_val arg with
          | Some code -> Second (Compiler.Ast.Exit code, Runtime.Null)
          | None ->
              let msg =
                Printf.sprintf
                  "The argument passed to `exit()` must be an integer, got %s"
                @@ Runtime.to_s arg
              in
              Second (Compiler.Ast.Error msg, Runtime.Null));
    };
    {
      name = "assert";
      arity = None;
      (* could be 1 or 2 *)
      cb =
        (fun args ->
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
          | Error err -> Second (Error err, Runtime.Null));
    };
  ]

type proto_t = {
  name : string;
  methods : func_t list;
  static_members : func_t list;
}

let make_protos m =
  let module M = (val m : Native.Sig) in
  [
    { name = "Number"; methods = []; static_members = [] };
    {
      name = "List";
      methods =
        [
          {
            name = "length";
            arity = Some 1;
            cb =
              (fun args ->
                let arg = List.hd_exn args in
                let arr_of_ts =
                  Runtime.list_of_val arg |> Option.value_exn ~message:__LOC__
                in
                let len = Array.length arr_of_ts |> Float.of_int in
                First (Runtime.Num len));
          };
        ];
      static_members = [];
    };
    {
      name = "Process";
      methods = [];
      static_members =
        [
          {
            name = "new";
            arity = Some 2;
            cb =
              (fun args ->
                let arg = List.nth_exn args 1 in
                match Runtime.list_of_val arg with
                | None ->
                    let err_msg =
                      Printf.sprintf
                        "Expected the first argument to `Process.new` to be a \
                         List[String] but got `%s`"
                      @@ Runtime.to_s arg
                    in
                    Second (Compiler.Ast.Error err_msg, Runtime.Null)
                | Some arr ->
                    let cmd_either =
                      List.of_array arr (* Not efficient *)
                      |> List.fold_right ~init:(First []) ~f:(fun t acc ->
                             match acc with
                             | First prev -> (
                                 let string_opt = Runtime.string_of_val t in
                                 match string_opt with
                                 | Some s -> First (s :: prev)
                                 | None ->
                                     Second
                                       ( Compiler.Ast.Error
                                           (Printf.sprintf
                                              "Expected the first argument to \
                                               `Process.new` to be a \
                                               List[String], but got a \
                                               non-String element"),
                                         Runtime.Null ))
                             | Second _ as sec -> sec)
                    in
                    Either.map cmd_either ~second:Fun.id ~first:(fun cmd ->
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
                        Runtime.Process proc));
          };
        ];
    };
    {
      name = "ProcessHandle";
      methods =
        [
          {
            name = "wait";
            arity = Some 1;
            cb =
              (fun args ->
                let handle = List.hd_exn args in
                let handle =
                  Runtime.process_handle_of_val handle |> Option.value_exn
                in
                let res =
                  match handle with
                  | ProcessInherited _ ->
                      Sloth_common.Common.internal_failure __LOC__
                  | ProcessBuffered _ -> M.wait handle
                in
                match res with
                | Ok proc_result -> First proc_result
                | Error msg -> Second (Compiler.Ast.Error msg, Runtime.Null));
          };
        ];
      static_members = [];
    };
    {
      name = "ProcessResult";
      methods =
        [
          {
            name = "stdout";
            arity = Some 1;
            cb =
              (fun args ->
                let proc = List.hd_exn args in
                let result =
                  Runtime.process_result_of_val proc |> Option.value_exn
                in
                First (Runtime.String result.stdout));
          };
        ];
      static_members = [];
    };
    {
      name = "HashMap";
      methods =
        [
          {
            name = "merge";
            arity = Some 2;
            cb =
              (fun args ->
                let left =
                  List.hd_exn args |> Runtime.hashmap_of_val |> Option.value_exn
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
                First (Runtime.HashMap left_copy));
          };
        ];
      static_members = [];
    };
    {
      name = "String";
      methods =
        [
          {
            name = "trim";
            arity = Some 1;
            cb =
              (fun args ->
                let str = List.hd_exn args in
                let ocaml_string =
                  Runtime.string_of_val str |> Option.value_exn
                in
                First (Runtime.String (String.strip ocaml_string)));
          };
        ];
      static_members = [];
    };
    {
      name = "Directory";
      methods =
        [
          {
            name = "exists";
            arity = Some 1;
            cb =
              (fun args ->
                let path =
                  List.hd_exn args |> Runtime.directory_of_val
                  |> Option.value_exn
                in
                let b = M.directory_exists path in
                First (Runtime.Bool b));
          };
          {
            name = "create";
            arity = Some 1;
            cb =
              (fun args ->
                let path =
                  List.hd_exn args |> Runtime.directory_of_val
                  |> Option.value_exn
                in
                M.mkdir path;
                First Runtime.Null);
          };
          {
            name = "path";
            arity = Some 1;
            cb =
              (fun args ->
                let path =
                  List.hd_exn args |> Runtime.directory_of_val
                  |> Option.value_exn
                in
                First (Runtime.String path));
          };
        ];
      static_members = [];
    };
    {
      name = "File";
      methods =
        [
          {
            name = "readString";
            arity = Some 2;
            cb =
              (fun args ->
                let first_arg = List.nth_exn args 1 in
                let Runtime.{ path } =
                  match Runtime.file_of_val first_arg with
                  | Some f -> f
                  | None -> Sloth_common.Common.internal_failure __LOC__
                in
                (* Errors? *)
                let contents = M.file_read_all path in
                First (Runtime.String contents));
          };
        ];
      static_members =
        [
          {
            name = "new";
            arity = Some 2;
            cb =
              (fun args ->
                let first_arg = List.nth_exn args 1 in
                (match Runtime.string_of_val first_arg with
                | None ->
                    Second
                      ( Compiler.Ast.Error
                          (Printf.sprintf
                             "You must pass a String to File.new(), but got a \
                              %s"
                          @@ Runtime.to_s first_arg),
                        Runtime.Null )
                | Some str -> First str)
                |> Either.map ~second:Fun.id ~first:(fun path ->
                       Runtime.File { path }));
          };
        ];
    };
  ]

let context_ids ~cwd ~env ~script_path ~argv =
  [
    ("$cwd", Runtime.String cwd);
    ("$env", Runtime.val_of_env env);
    ("$script", Runtime.String script_path);
    ("$scriptDir", Runtime.String (Filename.dirname script_path));
    ( "$argv",
      Runtime.List
        (List.to_array @@ List.map argv ~f:(fun s -> Runtime.String s)) );
  ]
