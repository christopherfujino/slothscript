open Core

type frame = { previous : frame option; values : string list }

(* TODO add types *)
type t = { src : string; ids : frame; protos : string list }

let create_frame previous = { previous; values = [] }
let push_empty env = { env with ids = create_frame (Some env.ids) }
let create src = { ids = create_frame None; src; protos = [] }
let src t' = t'.src

(* TODO pass a position here *)
let rec bind env name =
  let frame = env.ids in
  (* Check the current stack frame if it already exists *)
  match List.find frame.values ~f:(String.equal name) with
  | None ->
      let ids = { frame with values = name :: frame.values } in
      Some { env with ids }
  | Some _ -> None

and find env name =
  let frame = env.ids in
  let o = List.find frame.values ~f:(String.equal name) in
  match o with
  | Some n -> Some n
  | None -> (
      match frame.previous with
      | Some e -> (find [@tailrec]) { env with ids = e } name
      | None -> None)

let has_proto env name =
  List.find env.protos ~f:(String.equal name) |> Option.is_some

let populate env =
  let open Sloth_common.Stdlib_interface in
  let l = globals in
  List.fold_left ~init:env
    ~f:(fun acc (name, _) -> bind acc name |> Option.value_exn)
    l
