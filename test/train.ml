open Common
open Core

let () =
  let cb : string -> string * test_spec =
   fun path -> (path, Spec_parser.deserialize path)
  in
  find_child_specs "./green_specs"
  |> List.map ~f:cb
  |> List.iter ~f:(fun (path, spec) ->
         let pretty_ast = Printer.sexp_formatter spec.ast in
         if not (String.equal pretty_ast spec.ast) then
           Spec_parser.serialize path { spec with ast = pretty_ast })
