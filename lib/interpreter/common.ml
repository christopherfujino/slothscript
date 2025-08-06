exception Failure of string

let failure pos s =
  let pos_s = Sloth_common.Position.string_of_t pos in
  let msg = Printf.sprintf "[%s] %s" pos_s s in
  raise (Failure msg)
