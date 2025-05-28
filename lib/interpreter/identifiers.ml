type t = (string, Runtime.t) Hashtbl.t list ref

let create () = ref [ Hashtbl.create 8 ]

let set ids id v =
  let tbl = List.hd !ids in
  let maybe_val = Hashtbl.find_opt tbl id in
  match maybe_val with
  | Some _ ->
      let msg = Printf.sprintf "cannot rebind name %s" id in
      failwith msg
  | None -> Hashtbl.add tbl id v
