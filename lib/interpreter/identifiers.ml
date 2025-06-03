open Core

type 'a t = (string, 'a, String.comparator_witness) Base.Map.t

let create () = Map.empty (module String)

(* TODO figure out recursion when setting funcs *)
let set ids id v =
  Printf.printf "setting var %s\n" id;
  let maybe_val = Map.find ids id in
  match maybe_val with
  | Some _ ->
      let msg = Printf.sprintf "cannot rebind name %s" id in
      failwith msg
  | None -> Map.add_exn ids ~key:id ~data:v

let rec get_opt tbls id =
  Printf.printf "looking for %s in..." id;
  match tbls with
  | [] -> None
  | hd :: tl -> (
      Map.iter_keys hd ~f:print_string;
      (* Hashtbl.iter (fun k v -> Printf.printf "(%s=>%s) " k (Runtime.to_s v)) hd; *)
      Printf.printf "\n";
      (* Use monadic interface? *)
      match Map.find hd id with
      | Some v -> Some v
      | None -> (get_opt [@tailrec]) tl id)

let get ids id =
  match get_opt ids id with
  | Some v -> v
  | None -> Printf.sprintf "No variable defined named %s" id |> failwith
