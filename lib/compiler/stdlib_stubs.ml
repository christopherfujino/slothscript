open Core

let populate (env : Environment.t) =
  let l = [ "print" ] in
  List.fold_left ~init:env ~f:Environment.bind l
