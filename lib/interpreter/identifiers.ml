open Core

(* This type is polymorphic to avoid module cycle with Runtime *)
type 'a t = { previous : 'a t option; values : (string, 'a) Hashtbl.t }

let create () =
  { previous = None; values = Hashtbl.create ~size:8 (module String) }

let push_empty t' =
  { previous = Some t'; values = Hashtbl.create ~size:8 (module String) }

(* TODO figure out recursion when setting funcs *)
let bind env id v =
  let maybe_val = Hashtbl.find env.values id in
  match maybe_val with
  | Some _ -> None
  | None ->
      Hashtbl.add_exn ~key:id ~data:v env.values;
      Some ()

let rec get_opt env id =
  match Hashtbl.find env.values id with
  | None -> (
      match env.previous with
      | None -> None
      | Some prev -> (get_opt [@tailcall]) prev id)
  | Some v -> Some v

let get env id = match get_opt env id with Some v -> Some v | None -> None

let rec reassign env id v =
  (* TODO optimize to only use .change *)
  match Hashtbl.find env.values id with
  (* We don't care about the previous value, we just want to replace it *)
  | Some _ ->
      Hashtbl.change env.values id ~f:(fun _ -> Some v);
      Some ()
  | None -> (
      match env.previous with
      | Some prev -> (reassign [@tailcall]) prev id v
      | None -> None)

let rec debug_rec indent t' to_s =
  let keys = Hashtbl.keys t'.values in
  List.iter keys ~f:(fun key ->
      let value = Hashtbl.find_exn t'.values key in
      let value_s = to_s value in
      let indent_s = String.make indent ' ' in
      (* TODO recurse for prev *)
      Printf.printf "%s%s => %s\n" indent_s key value_s);
  match t'.previous with
  | None -> ()
  | Some prev -> debug_rec (indent + 2) prev to_s

let debug t' to_s = debug_rec 0 t' to_s
