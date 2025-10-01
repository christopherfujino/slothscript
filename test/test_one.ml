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
            spec.program test ~env:[| "UNIT_TEST=true" |]
        in

        let _, _ = Interpreter.Interpret.interpret_prog globals ir in
        let forward_buffer = List.rev !M.stdout_buffer in
        let catted_output_opt =
          List.fold_left forward_buffer
            ~f:(fun acc cur ->
              Some (match acc with None -> cur | Some acc -> acc ^ cur))
            ~init:None
        in
        (* Is STDOUT correct? *)
        let is_equal, catted_output =
          match catted_output_opt with
          | None -> (String.(spec.stdout_expect = ""), "")
          | Some s -> (String.(spec.stdout_expect = String.strip s), s)
        in
        if is_equal then ()
        else
          Printf.eprintf
            "STDOUT did not match expectations\n\nExpected %s\n\nReceived: %s\n"
            spec.stdout_expect catted_output; failwith "Fail")
  in
  match res with
  | Ok () -> ()
  | Error msg ->
      Out_channel.flush stdout;
      Printf.eprintf "%s\n" msg;
      exit 1
