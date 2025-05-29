type t = (string, Runtime.t) Hashtbl.t list ref

let create () = ref [ Hashtbl.create 8 ]

let set ids id v =
  let tbl = List.hd !ids in
  let maybe_val = Hashtbl.find_opt tbl id in
  match maybe_val with
  | Some _ ->
      let msg = Printf.sprintf "cannot rebind name %s" id in
      failwith msg
  | None -> Printf.printf "setting name %s\n" id; Hashtbl.add tbl id v

let rec get_from_tables tbls id =
  match tbls with
  | [] -> Printf.sprintf "Unknown identifier %s" id |> failwith
  | hd :: tl -> (
      match Hashtbl.find_opt hd id with
      | Some v -> v
      | None -> (get_from_tables [@tailrec]) tl id)

let get ids id = get_from_tables !ids id
