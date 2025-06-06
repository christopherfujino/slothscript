open Core

(* This type is polymorphic to avoid module cycle with Runtime *)
type 'a t = (string, 'a) Hashtbl.t

let create () = Hashtbl.create ~size:8 (module String)

(* TODO figure out recursion when setting funcs *)
let set ids id v =
  Printf.printf "setting var %s\n" id;
  let maybe_val = Hashtbl.find ids id in
  match maybe_val with
  | Some _ ->
      let msg = Printf.sprintf "cannot rebind name %s" id in
      failwith msg
  | None -> Hashtbl.add_exn ~key:id ~data:v ids

let rec get_opt tbls id =
  match tbls with
  | [] -> None
  | hd :: tl -> (
      (* Use monadic interface? *)
      match Hashtbl.find hd id with
      | Some v -> Some v
      | None -> (get_opt [@tailrec]) tl id)

let get ids id =
  match get_opt ids id with
  | Some v -> v
  | None -> Printf.sprintf "No variable defined named %s" id |> failwith

let reassign _ _ _ = failwith "YOLO"
