type frame = (string, Runtime.t) Hashtbl.t
type t = frame list

let create () = Hashtbl.create 8

let set ids id v =
  let tbl = List.hd ids in
  let maybe_val = Hashtbl.find_opt tbl id in
  match maybe_val with
  | Some _ ->
      let msg = Printf.sprintf "cannot rebind name %s" id in
      failwith msg
  | None -> Hashtbl.add tbl id v

let rec get_opt tbls id =
  match tbls with
  | [] -> None
  | hd :: tl -> (
      (* Use monadic interface? *)
      match Hashtbl.find_opt hd id with
      | Some v -> Some v
      | None -> (get_opt [@tailrec]) tl id)

let get ids id =
  match get_opt ids id with
  | Some v -> v
  | None -> Printf.sprintf "No variable defined named %s" id |> failwith

let push_new_frame t' = create () :: t'

let pop t' =
  match t' with [] -> failwith "can't pop an empty stack!" | _ :: tl -> tl
