open Interpreter

let wrap_interpret ctx prog =
  try Interpret.interpret_prog ctx prog
  with Common.Failure msg ->
    Printf.fprintf stderr "%s\n" msg;
    exit 1

let wrap_parse env line =
  try Compiler.Main.parse env line
  with Compiler.Optimizer.Failure msg ->
    Printf.fprintf stderr "%s\n" msg;
    exit 1

let repl () =
  (* TODO history file *)
  Readline.init ();
  let ctx = Context.make_ctx (module Sloth_stdlib.Prod) "" in
  let env = Compiler.Environment.create "" |> Compiler.Environment.populate in
  let rec repl_inner ctx env =
    let line =
      (* TODO autocomplete *)
      let line_opt = Readline.readline ~completion_fun:(fun foo -> Readline.Custom []) ~prompt:"> " () in
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
    let env, prog = wrap_parse env line in
    let ctx, v = wrap_interpret ctx prog in
    Runtime.to_s v |> print_endline;
    (repl_inner [@tailcall]) ctx env
  in
  repl_inner ctx env

let interpreter () =
  let rec read_all buf =
    let cur_line_opt = try Some (read_line ()) with End_of_file -> None in
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
  let _, ir = wrap_parse env program in
  let _, _ = wrap_interpret ctx ir in
  ()

let () = if Unix.isatty Unix.stdin then repl () else interpreter ()
