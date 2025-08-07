open Core

let populate (env : Environment.t) =
  let l = [ "print" ] in
  List.fold_left ~init:env
    ~f:(fun acc cur -> Environment.bind acc cur |> Option.value_exn)
    l
