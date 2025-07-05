open Common
open Core

let () =
  find_child_specs "./green_specs"
  |> List.map ~f:Spec_parser.deserialize
  |> List.iter ~f:(fun spec -> Printf.printf "%s => %s\n" spec.name spec.ast)
