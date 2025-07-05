open Common
open Core

let () =
  find_child_specs "./green_specs"
  |> List.map ~f:Spec_parser.deserialize
  |> List.iter ~f:(fun spec -> print_endline spec.ast)
