let rec repl ctx_opt =
  let ctx =
    match ctx_opt with
    | None ->
        Interpreter.Context.make_ctx (module Interpreter.Sloth_stdlib.Prod)
    | Some c -> c
  in
  (* %! means flush *)
  Printf.printf "> %!";
  let line =
    try read_line ()
    with End_of_file ->
      Printf.printf "\n";
      exit 0
  in
  let prog = Compiler.Main.parse line in
  let open Interpreter in
  (* TODO this isn't a true REPL cos we can't print *)
  let ctx = Interpret.interpret_prog ctx prog in
  (repl [@tailcall]) (Some ctx)

let rec interpret () =
  let line =
    (* TODO We should read the whole doc at this point *)
    try read_line ()
    with End_of_file ->
      Printf.printf "\n";
      exit 0
  in
  let prog = Compiler.Main.parse line in
  let open Interpreter in
  let _ =
    Interpret.interpret_prog (Context.make_ctx (module Sloth_stdlib.Prod)) prog
  in
  (interpret [@tailcall]) ()

let () = if Unix.isatty Unix.stdin then repl None else interpret ()
