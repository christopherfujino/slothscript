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
        let globals =
          Interpreter.Globals.make_globals
            (module Interpreter.Native.Prod)
            spec.program test
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
