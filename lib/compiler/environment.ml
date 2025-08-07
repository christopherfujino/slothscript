open Core

(* TODO add types *)
type t = { previous : t option; values : string list; src : string }

let push_empty env = { env with previous = Some env; values = [] }
let create src = { previous = None; values = []; src }

let rec bind env name =
  (* Check the current stack frame if it already exists *)
  match List.find env.values ~f:(String.equal name) with
  | None -> Some { env with values = name :: env.values }
  | Some _ -> None

and find env name =
  let o = List.find env.values ~f:(String.equal name) in
  match o with
  | Some n -> Some n
  | None -> (
      match env.previous with
      | Some e -> (find [@tailrec]) e name
      | None -> None)
