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
        let _, ir = Compiler.Main.parse env spec.program in

        let proc_spec =
          Interpreter.Mock_process.spec_of_string spec.proc_spec
        in

        let m = Interpreter.Native.make_test proc_spec in
        let module M = (val m) in
        let globals =
          Interpreter.Globals.make_globals
            (module M)
            spec.program test ~env:[| "UNIT_TEST=true" |] ~argv:[]
        in

        let _, either = Interpreter.Interpret.interpret_prog globals ir in
        (match either with
        | First _ -> ()
        | Second bt -> (
            match bt with
            | Exit _ -> ()
            | Error msg -> Printf.eprintf "%s\n" @@ Interpreter.Runtime.to_s msg; exit 1
            | Return _ | Break _ | Continue _ -> failwith "Unreachable"));

        let stdout = M.get_stdout () |> String.strip in
        (* Is STDOUT correct? *)
        let is_equal = String.(spec.stdout_expect = stdout) in

        if is_equal then ()
        else (
          Printf.eprintf
            "STDOUT did not match expectations\n\n\
             Expected %s\n\n\
             Received: %s\n\
             %!"
            spec.stdout_expect stdout;
          exit 1))
  in
  match res with
  | Ok () -> ()
  | Error msg ->
      Out_channel.flush stdout;
      Printf.eprintf "%s\n" msg;
      exit 1
