open Core

(* TODO add types *)
type t = { previous : t option; values : string list }

let bind env name = { env with values = name :: env.values }
let push_empty env = { previous = Some env; values = [] }
let create () = { previous = None; values = [] }

let rec find env name =
  let o = List.find env.values ~f:(String.equal name) in
  match o with
  | Some n -> Some n
  | None -> (
      match env.previous with
      | Some e -> (find [@tailrec]) e name
      | None -> None)
