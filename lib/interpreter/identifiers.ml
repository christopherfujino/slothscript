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
  | Some _ ->
      let msg = Printf.sprintf "cannot rebind name %s" id in
      raise (Common.Failure msg)
  | None ->
      Printf.printf "Binding %s\n" id (* TODO delete debug print *);
      Hashtbl.add_exn ~key:id ~data:v env.values

let rec get_opt env id =
  match Hashtbl.find env.values id with
  | None -> (
      match env.previous with
      | None -> None
      | Some prev -> (get_opt [@tailcall]) prev id)
  | Some v -> Some v

let get env id =
  match get_opt env id with
  | Some v -> v
  | None ->
      let msg = Printf.sprintf "No variable defined named %s" id in
      raise (Common.Failure msg)

let rec reassign env id v =
  match Hashtbl.find env.values id with
  (* We don't care about the previous value, we just want to replace it *)
  | Some _ -> Hashtbl.change env.values id ~f:(fun _ -> Some v)
  | None -> (
      match env.previous with
      | Some prev -> (reassign [@tailcall]) prev id v
      | None ->
          Printf.sprintf
            "No previous variable named %s; did you intend to declare a new \
             variable?"
            id
          |> failwith)
