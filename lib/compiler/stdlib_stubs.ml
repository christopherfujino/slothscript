open Core

let populate (env : Environment.t) =
  let open Sloth_common.Stdlib_interface in
  let l = globals in
  List.fold_left ~init:env
    ~f:(fun acc (name, _) -> Environment.bind acc name |> Option.value_exn)
    l
