open Common
open Core

let () =
  let errors =
    find_child_specs "./green_specs"
    |> List.map ~f:Spec_parser.deserialize
    |> List.map ~f:(function
         | Valid spec ->
             Printf.printf "%s => %s\n" spec.name spec.ast;
             None
         | Invalid reason -> Some reason)
    |> List.filter ~f:(function None -> false | Some _ -> true)
  in
  if List.length errors > 0 then
    List.iter errors ~f:(fun opt -> Option.value_exn opt |> print_endline)
