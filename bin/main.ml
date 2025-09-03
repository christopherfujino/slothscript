open Core
open Interpreter

let wrap_interpret ctx prog =
  try Some (Interpret.interpret_prog ctx prog)
  with Common.Failure msg ->
    (* We probably don't need a stacktrace from Interpret.interpret_prog to here *)
    Printf.fprintf stderr "%s%!" msg;
    None

let wrap_parse env line =
  try Some (Compiler.Main.parse env line)
  with Compiler.Common.ParserFailure msg ->
    (* We probably don't need a stacktrace from Compiler.Main.parse to here *)
    Printf.fprintf stderr "%s%!" msg;
    None

let repl () =
  (match Sys.getenv "HOME" with
  | None -> Readline.init ()
  | Some home ->
      let history_file = Printf.sprintf "%s/.sloth_repl.history" home in
      Readline.init ~history_file ());

  let globals = Globals.make_globals (module Sloth_stdlib.Prod) "" in
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

    let ( let* ) o f = Option.bind o ~f in

    let opt =
      let* env, prog = wrap_parse env line in
      let* globals, v = wrap_interpret globals prog in
      Runtime.to_s v |> print_endline;
      Option.return (globals, env)
    in

    let globals, env =
      match opt with Some (ctx, env) -> (ctx, env) | None -> (globals, env)
    in
    (repl_inner [@tailcall]) globals env
  in
  repl_inner globals env

let interpreter path =
  let program = In_channel.read_all path in
  let env =
    Compiler.Environment.create program |> Compiler.Environment.populate
  in
  let globals = Globals.make_globals (module Sloth_stdlib.Prod) program in

  let opt_ir = wrap_parse env program in
  let code =
    match opt_ir with
    | None -> 1
    | Some (_, ir) -> (
        let opt = wrap_interpret globals ir in
        match opt with Some _ -> 0 | None -> 1)
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
