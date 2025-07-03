let rec repl env_opt ctx_opt =
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
  (* TODO this isn't a true REPL cos we can't print *)
  let ctx = Interpret.interpret_prog ctx prog in
  (repl [@tailcall]) (Some env) (Some ctx)

let () =
  if Unix.isatty Unix.stdin then repl None None
  else failwith "TODO implement script interpreter"
