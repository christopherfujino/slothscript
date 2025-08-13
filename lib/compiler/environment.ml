open Core

type frame = { previous : frame option; values : string list }

(* TODO add types *)
type t = { src : string; ids : frame }

let create_frame previous = { previous; values = [] }
let push_empty env = { env with ids = create_frame (Some env.ids) }
let create src = { ids = create_frame None; src }
let src t' = t'.src

(* TODO pass a position here *)
let bind env name =
  let frame = env.ids in
  (* Check the current stack frame if it already exists *)
  match List.find frame.values ~f:(String.equal name) with
  | None ->
      let ids = { frame with values = name :: frame.values } in
      Some { env with ids }
  | Some _ -> None

let rec find env name =
  let frame = env.ids in
  let o = List.find frame.values ~f:(String.equal name) in
  match o with
  | Some n -> Some n
  | None -> (
      match frame.previous with
      | Some e -> (find [@tailrec]) { env with ids = e } name
      | None -> None)

let populate env =
  let open Sloth_common.Stdlib_interface in
  let l = globals in
  let env =
    List.fold_left ~init:env
      ~f:(fun acc name -> bind acc name |> Option.value_exn)
      l.ids
  in
  List.fold_left ~init:env
    ~f:(fun env { name; methods = _; static_members = _ } ->
      bind env name |> Option.value_exn)
    l.protos
