open Core

let rec todo_get ids name =
  match ids with
  | [] -> None
  | hd :: tl -> (
      match Hashtbl.find hd name with
      | Some _ as v_opt -> v_opt
      | None -> (todo_get [@tailcall]) tl name)
