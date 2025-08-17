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

  let ctx = Context.make_ctx (module Sloth_stdlib.Prod) "" in
  let env = Compiler.Environment.create "" |> Compiler.Environment.populate in
  let rec repl_inner ctx env =
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
    let ctx = Context.{ ctx with src = line } in

    let ( let* ) o f = Option.bind o ~f in

    let opt =
      let* env, prog = wrap_parse env line in
      let* ctx, v = wrap_interpret ctx prog in
      Runtime.to_s v |> print_endline;
      Option.return (ctx, env)
    in

    let ctx, env =
      match opt with Some (ctx, env) -> (ctx, env) | None -> (ctx, env)
    in
    (repl_inner [@tailcall]) ctx env
  in
  repl_inner ctx env

let interpreter () =
  let rec read_all buf =
    let cur_line_opt =
      try
        Out_channel.(flush stdout);
        Some In_channel.(input_line_exn stdin)
      with End_of_file -> None
    in
    match cur_line_opt with
    | None -> Buffer.contents buf
    | Some cur_line ->
        Buffer.add_string buf cur_line;
        Buffer.add_char buf '\n';
        (read_all [@tailcall]) buf
  in
  let program = read_all (Buffer.create 256) in
  let env =
    Compiler.Environment.create program |> Compiler.Environment.populate
  in
  let ctx = Context.make_ctx (module Sloth_stdlib.Prod) program in

  let opt_ir = wrap_parse env program in
  let code =
    match opt_ir with
    | None -> 1
    | Some (_, ir) -> (
        let opt = wrap_interpret ctx ir in
        match opt with Some _ -> 0 | None -> 1)
  in
  exit code

let () = if Core_unix.isatty Core_unix.stdin then repl () else interpreter ()
