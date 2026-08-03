open Core
open Interpreter
open Sloth_common.Common
open Native_host_api

let parse_compile_interpret (prog, s) =
  let argv = Sys.get_argv () |> Array.to_list in
  let program_name = List.hd_exn argv in
  let env = Compiler.Environment.create prog |> Compiler.Environment.populate in
  let globals =
    Globals.make_globals
      (module Native.Prod)
      ~argv:(List.drop argv 1) prog program_name ~env:(Core_unix.environment ())
      ~version
  in
  state_add s '\042';
  let _, ast = Compiler.Main.parse env prog in
  let _, either = Interpreter.Interpret.interpret_prog globals ast in
  match either with
  | First final_val -> final_val
  | Second _ -> failwith "Fail!"

let init_repl_env () =
  let argv = Sys.get_argv () |> Array.to_list in
  let program_name = List.hd_exn argv in
  let env = Compiler.Environment.create "" |> Compiler.Environment.populate in
  let globals =
    Globals.make_globals
      (module Native.Prod)
      ~argv:(List.drop argv 1) "" program_name ~env:(Core_unix.environment ())
      ~version
  in
  (env, globals)

let parse_compile_interpret_print ((env, globals), prog) =
  let env = Compiler.Environment.update_src env prog in
  let globals = Globals.{ globals with src = prog } in
  let env, ast = Compiler.Main.parse env prog in
  let globals, either = Interpreter.Interpret.interpret_prog globals ast in
  match either with
  | First v ->
      Runtime.to_s v |> print_endline;
      (env, globals)
  | Second bt -> (
      match bt with
      | Exit code -> exit code
      | Error (msg_opt, _) ->
          let msg = Option.value_exn msg_opt in
          Printf.fprintf stderr "%s\n%!" msg;
          (env, globals)
      | _ -> failwith "TODO")

let () =
  Callback.register "init_repl_env" init_repl_env;
  Callback.register "parse_compile_interpret" parse_compile_interpret;
  Callback.register "parse_compile_interpret_print"
    parse_compile_interpret_print
