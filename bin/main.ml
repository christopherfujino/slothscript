open Core
open Interpreter
open Sloth_common.Common

(*
let wrap_interpret globals prog =
  match
    Sloth_common.Common.wrap_error (fun () ->
        Interpret.interpret_prog globals prog)
  with
  | Ok v -> Some v
  | Error msg ->
      (* Ensure stdout is written before error handling *)
      Out_channel.flush stdout;
      (* We probably don't need a stacktrace from Interpret.interpret_prog to here *)
      Printf.fprintf stderr "\n%s%!" msg;
      None

let wrap_parse env line =
  try Some (Compiler.Main.parse env line)
  with Compiler.Common.ParserFailure msg ->
    (* We probably don't need a stacktrace from Compiler.Main.parse to here *)
    Printf.fprintf stderr "%s%!" msg;
    None
*)

let ( let* ) r f = Result.bind r ~f

let repl () =
  (match Sys.getenv "HOME" with
  | None -> Readline.init ()
  | Some home ->
      let history_file = Printf.sprintf "%s/.sloth_repl.history" home in
      Readline.init ~history_file ());

  let globals = Globals.make_globals (module Native.Prod) "" "REPL" in
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
    let env = Compiler.Environment.update_src env line in
    let globals = Globals.{ globals with src = line } in

    let res =
      let* env, prog = wrap_error (fun () -> Compiler.Main.parse env line) in
      let* globals, v =
        wrap_error (fun () -> Interpreter.Interpret.interpret_prog globals prog)
      in
      print_endline @@ Runtime.to_s v;
      Result.return (globals, env)
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

let interpreter path =
  let program = In_channel.read_all path in
  let env =
    Compiler.Environment.create program |> Compiler.Environment.populate
  in
  let globals = Globals.make_globals (module Native.Prod) program path in

  let result =
    let* _, prog = wrap_error (fun () -> Compiler.Main.parse env program) in
    let* _ =
      wrap_error (fun () -> Interpreter.Interpret.interpret_prog globals prog)
    in
    Result.return 0
  in

  let code =
    match result with
    | Error msg ->
        Out_channel.flush stdout;
        Printf.eprintf "%s" msg;
        1
    | Ok code -> code
  in
  exit code

let () =
  let argv = Sys.get_argv () in
  let argc = Array.length argv in
  match argc with
  | 2 ->
      let script = Array.get argv 1 in
      interpreter script
  | 1 -> repl ()
  | _ -> Printf.sprintf "TODO: implement sub-commands" |> failwith
