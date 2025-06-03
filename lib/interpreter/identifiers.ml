type 'a t = (string, 'a) Hashtbl.t

let create () = Hashtbl.create 8

let set ids id v =
  (*Printf.printf "setting %s to %s\n" id (Runtime.to_s v); *)
  let tbl = List.hd ids in
  let maybe_val = Hashtbl.find_opt tbl id in match maybe_val with
  | Some _ ->
      let msg = Printf.sprintf "cannot rebind name %s" id in
      failwith msg
  | None -> Hashtbl.add tbl id v

let rec get_opt tbls id =
  Printf.printf "looking for %s in..." id;
  match tbls with
  | [] -> None
  | hd :: tl -> (
      (* Hashtbl.iter (fun k v -> Printf.printf "(%s=>%s) " k (Runtime.to_s v)) hd;
      Printf.printf "\n"; *)
      (* Use monadic interface? *)
      match Hashtbl.find_opt hd id with
      | Some v -> Some v
      | None -> (get_opt [@tailrec]) tl id)

let get ids id =
  match get_opt ids id with
  | Some v -> v
  | None -> Printf.sprintf "No variable defined named %s" id |> failwith
