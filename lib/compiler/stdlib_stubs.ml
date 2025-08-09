open Core

let populate (env : Environment.t) =
  let l = Sloth_common.Stdlib_interface.globals in
  List.fold_left ~init:env
    ~f:(fun acc cur -> Environment.bind acc cur.name |> Option.value_exn)
    l
