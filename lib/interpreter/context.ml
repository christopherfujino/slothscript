open Core

type t = (string, Runtime.t) Hashtbl.t list

let tbl_size = 10
let create () = [ Hashtbl.create ~size:tbl_size (module String) ]

let rec get ids name =
  match ids with
  | [] -> None
  | hd :: tl -> (
      match Hashtbl.find hd name with
      | Some _ as v_opt -> v_opt
      | None -> (get [@tailcall]) tl name)
