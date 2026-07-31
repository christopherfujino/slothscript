open Core

(* Polymorphic to avoid module cycle with Runtime *)
type 'a t = (string, 'a) Hashtbl.t list

let tbl_size = 10
let create () = [ Hashtbl.create ~size:tbl_size (module String) ]

(* TODO this can be smaller *)
let push_empty prev = Hashtbl.create ~size:tbl_size (module String) :: prev

let rec get t' name =
  match t' with
  | [] -> None
  | hd :: tl -> (
      match Hashtbl.find hd name with
      | Some _ as v_opt -> v_opt
      | None -> (get [@tailcall]) tl name)

let bind t' name v =
  match get t' name with
  | None ->
      let tbl = List.hd_exn t' in
      Hashtbl.add_exn tbl ~key:name ~data:v;
      Some ()
  | Some _ -> None

let reassign t' name v =
  match get t' name with
  | None -> None
  | Some _ ->
      (* No matter where we found it, declare it at the top scope *)
      let tbl = List.hd_exn t' in
      Hashtbl.change tbl name ~f:(fun _ -> Some v);
      Some ()
