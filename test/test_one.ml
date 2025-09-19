open Common
open Core

let () =
  let argv = Sys.get_argv () in
  let argc = Array.length argv in
  let test =
    match argc with
    | 2 -> Array.get argv 1
    | _ ->
        Printf.sprintf "Usage: pass a single test spec path as a CLI arg"
        |> failwith
  in
  let spec = Spec_parser.deserialize test in

  let env =
    Compiler.Environment.create spec.program |> Compiler.Environment.populate
  in

  let res =
    Sloth_common.Common.wrap_error (fun () ->
        let proc_spec =
          Interpreter.Mock_process.spec_of_string spec.proc_spec
        in
        let m = Interpreter.Native.make_test proc_spec in
        let module M = (val m) in
        let globals =
          Interpreter.Globals.make_globals (module M) spec.program test ~env:[|"UNIT_TEST=true"|]
        in

        let _, ir = Compiler.Main.parse env spec.program in
        let _, _ = Interpreter.Interpret.interpret_prog globals ir in
        ())
  in
  match res with
  | Ok () -> ()
  | Error msg ->
      Printf.eprintf "%s" msg;
      exit 1
