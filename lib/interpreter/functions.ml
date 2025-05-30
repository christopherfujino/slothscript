type t = (string, Runtime.function_t) Hashtbl.t

let create () = Hashtbl.create 8

let set funcs id f =
  (* TODO assert this is actually a func *)
  let precursor_opt = Hashtbl.find_opt funcs id in
  match precursor_opt with
  | Some _ ->
      Printf.sprintf "A function named \"%s\" has already been defined" id
      |> failwith
  | None -> Hashtbl.add funcs id f
