open Core
open Interpreter
open Sloth_common.Common

let ( let* ) r f = Result.bind r ~f

let repl () =
  (match Sys.getenv "HOME" with
  | None -> Readline.init ()
  | Some home ->
      let history_file = Printf.sprintf "%s/.sloth_repl.history" home in
      Readline.init ~history_file ());

  let globals =
    Globals.make_globals
      (module Native.Prod)
      "" "REPL" ~env:(Core_unix.environment ()) ~argv:[]
  in
  let env = Compiler.Environment.create "" |> Compiler.Environment.populate in
  let rec repl_inner globals env =
    let line =
      (* TODO autocomplete *)
      let line_opt = Readline.readline ~prompt:"> " () in
      match line_opt with
      | None ->
          Printf.printf "\n";
          exit 0
      | Some line ->
          Readline.add_history line;
          line
    in

    if Sloth_common.Common.debug_mode then Compiler.Main.debug line else ();

    let env = Compiler.Environment.update_src env line in
    let globals = Globals.{ globals with src = line } in

    let res =
      let* env, prog = wrap_error (fun () -> Compiler.Main.parse env line) in
      let* globals, v =
        wrap_error (fun () -> Interpreter.Interpret.interpret_prog globals prog)
      in
      match v with
      | First v ->
          print_endline @@ Runtime.to_s v;
          Result.return (globals, env)
      | Second (bt, _) -> (
          match bt with
          | Error msg -> Result.Error msg
          | Exit code -> exit code
          | Return | Break | Continue ->
              Sloth_common.Common.internal_failure __LOC__)
    in

    let globals, env =
      match res with
      | Ok (ctx, env) -> (ctx, env)
      | Error msg ->
          Printf.fprintf stderr "%s\n%!" msg;
          (globals, env)
    in
    (repl_inner [@tailcall]) globals env
  in
  repl_inner globals env

let interpreter path argv =
  let program = In_channel.read_all path in
  let env =
    Compiler.Environment.create program |> Compiler.Environment.populate
  in
  let globals =
    Globals.make_globals
      (module Native.Prod)
      ~argv program path ~env:(Core_unix.environment ())
  in

  let result =
    let* _, prog = wrap_error (fun () -> Compiler.Main.parse env program) in
    let* _, either =
      wrap_error (fun () -> Interpreter.Interpret.interpret_prog globals prog)
    in
    match either with
    | First _ -> Ok 0
    | Second (bt, _) -> (
        match bt with
        | Exit code -> Ok code
        | Error msg -> Error msg
        | Return | Break | Continue ->
            Sloth_common.Common.internal_failure __LOC__)
  in

  let code =
    match result with
    | Error msg ->
        Out_channel.flush stdout;
        Printf.eprintf "%s\n" msg;
        1
    | Ok code -> code
  in
  exit code

let () =
  let argv = Sys.get_argv () in
  let argc = Array.length argv in
  match argc with
  | 1 -> repl ()
  | _ when argc > 1 ->
      let script = Array.get argv 1 in
      let actual_args = Array.to_list argv |> List.sub ~pos:2 ~len:(argc - 2) in
      interpreter script actual_args
  | _ -> Sloth_common.Common.internal_failure __LOC__
