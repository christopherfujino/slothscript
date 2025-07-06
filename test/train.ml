open Common
open Core

let () =
  let spec_results =
    find_child_specs "./green_specs"
    |> List.map ~f:Spec_parser.deserialize
  in
  let _, bad_specs = List.partition_result spec_results in
  if List.length bad_specs > 0 then
    List.iter bad_specs ~f:(print_endline)
