open Common
open Core

let f spec_dir i arg =
  if i = 0 then
    (* args[0] is the bin name *)
    ()
  else
    let spec = Spec_parser.deserialize arg in
    let _, prog =
      (Compiler.Environment.create ()
      |> Compiler.Stdlib_stubs.populate |> Compiler.Main.parse)
      @@ spec.program
    in
    let canonical_ast =
      Compiler.Optimizer.prog_to_str prog |> Printer.sexp_formatter
    in
    let pretty_ast = Printer.sexp_formatter spec.ast in
    let state =
      if String.equal canonical_ast spec.ast then `Good
      else if String.equal canonical_ast pretty_ast then `Ugly
      else `Bad
    in
    let src_path = Printf.sprintf "%s/%s" spec_dir arg in
    (match state with
    | `Good -> ()
    | `Bad ->
        Printf.printf "[Bad]  %s\n" arg;
        Spec_parser.serialize src_path { spec with ast = pretty_ast }
    | `Ugly ->
        Printf.printf "[Ugly] %s\n" arg;
        Spec_parser.serialize src_path { spec with ast = pretty_ast });
    ()

let () =
  let spec_dir =
    match Sys.getenv "TEST_DIR" with
    | None -> failwith "You must set the TEST_DIR env var"
    | Some d -> d
  in
  let args = Sys.get_argv () in
  if Array.length args = 1 then failwith "Usage: train.exe [SPEC_PATH]+";
  Array.iteri args ~f:(f spec_dir)
