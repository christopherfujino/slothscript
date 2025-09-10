open Core
open Interpreter

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
  let spec = Test.Spec_parser.deserialize test in

  let env =
    Compiler.Environment.create spec.program |> Compiler.Environment.populate
  in
  let globals = Globals.make_globals (module Sloth_stdlib.Make_test ()) spec.program in

  let _, ir = Compiler.Main.parse env spec.program in
  let _, _ = Interpret.interpret_prog globals ir in
  ()
