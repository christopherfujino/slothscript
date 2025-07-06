open Common
open Core

let () =
  let spec_results =
    find_child_specs "./green_specs"
    |> List.map ~f:Spec_parser.deserialize
    |> List.map ~f:(function
         | Ok spec ->
             Printf.printf "%s => %s\n" spec.name spec.ast;
             None
         | Error reason -> Some reason)
  in
  let errors =
    List.filter spec_results ~f:(function None -> false | Some _ -> true)
  in
  if List.length errors > 0 then
    List.iter errors ~f:(fun opt -> Option.value_exn opt |> print_endline)
