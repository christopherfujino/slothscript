open Common
open Core

let () =
  let spec_dir =
    match Sys.getenv "TEST_DIR" with
    | None -> failwith "You must set the TEST_DIR env var"
    | Some d -> d
  in
  let specs = Specs.green () in
  List.iter specs ~f:(fun spec ->
      let name = spec.name in
      let buf = Buffer.create 20 in
      Printf.sprintf "%s/green_specs/" spec_dir |> Buffer.add_string buf;
      String.iter name ~f:(fun c ->
          match c with
          | ' ' -> Buffer.add_char buf '_'
          | '-' -> Buffer.add_char buf '_'
          | '/' -> Buffer.add_char buf '_'
          | _ -> Buffer.add_char buf c);
      Buffer.add_string buf ".sloth";
      let file_path = Buffer.contents buf in
      (match Core_unix.access file_path [ `Exists ] with
      | Ok () ->
          Printf.sprintf "The file %s already exists!" file_path |> failwith
      | Error _ -> ());
      Printf.printf "Serializing %s...\n" file_path;
      Spec_parser.serialize file_path spec)
(* TODO uncomment
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
          *)
