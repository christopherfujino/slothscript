let repl () =
  let ctx =
    Interpreter.Context.make_ctx (module Interpreter.Sloth_stdlib.Prod)
  in
  let env = Compiler.Environment.create () |> Compiler.Stdlib_stubs.populate in
  let rec repl_inner ctx env =
    (* %! means flush *)
    Printf.printf "> %!";
    let line =
      try read_line ()
      with End_of_file ->
        Printf.printf "\n";
        exit 0
    in
    let env, prog = Compiler.Main.parse env line in
    let open Interpreter in
    let ctx, v = Interpret.interpret_prog ctx prog in
    Runtime.to_s v |> print_endline;
    (repl_inner [@tailcall]) ctx env
  in
  repl_inner ctx env

let interpreter env_opt ctx_opt =
  let ctx =
    match ctx_opt with
    | None ->
        Interpreter.Context.make_ctx (module Interpreter.Sloth_stdlib.Prod)
    | Some c -> c
  in
  let env =
    match env_opt with
    | Some e -> e
    | None -> Compiler.Environment.create () |> Compiler.Stdlib_stubs.populate
  in
  let rec read_all buf =
    let cur_line_opt = try Some (read_line ()) with End_of_file -> None in
    match cur_line_opt with
    | None -> Buffer.contents buf
    | Some cur_line ->
        Buffer.add_string buf cur_line;
        Buffer.add_char buf '\n';
        (read_all [@tailcall]) buf
  in
  let program = read_all (Buffer.create 16) in
  print_endline program;
  let _, ir = Compiler.Main.parse env program in
  let _ = Interpreter.Interpret.interpret_prog ctx ir in
  ()

let () = if Unix.isatty Unix.stdin then repl () else interpreter None None
