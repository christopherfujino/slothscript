open Common
open Core

let () =
  let spec_dir =
    match Sys.getenv "TEST_DIR" with
    | None -> failwith "You must set the TEST_DIR env var"
    | Some d -> d
  in
  let args = Sys.get_argv () in
  Array.iteri args ~f:(fun i arg ->
      print_endline arg;
      if i = 0 then
        (* args[0] is the bin name *)
        ()
      else
        let spec = Spec_parser.deserialize arg in
        let pretty_ast = Printer.sexp_formatter spec.ast in
        if not (String.equal pretty_ast spec.ast) then (
          let src_path = Printf.sprintf "%s/%s" spec_dir arg in
          Spec_parser.serialize src_path { spec with ast = pretty_ast }))
