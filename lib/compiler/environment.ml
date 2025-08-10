open Core

(* TODO add types *)
type t = { previous : t option; values : string list; src : string }

let push_empty env = { env with previous = Some env; values = [] }
let create src = { previous = None; values = []; src }

(* TODO pass a position here *)
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

let populate env =
  let open Sloth_common.Stdlib_interface in
  let l = globals in
  List.fold_left ~init:env
    ~f:(fun acc (name, _) -> bind acc name |> Option.value_exn)
    l
