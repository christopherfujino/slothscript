type t =
  | String of string
  | Bool of bool
  | Num of float
  (* TODO make this a custom dynamic array *)
  | Array of t list
  (* TODO make this more efficient *)
  | Map of (string * t) list
  | Null
(* TODO hashmap *)

let rec to_s = function
  | String s -> Printf.sprintf "\"%s\"" s
  (* TODO write a custom, better Float.to_string *)
  | Num f -> Float.to_string f
  | Array l ->
      let is_first = ref true in
      let cb acc cur =
        if !is_first then (
          is_first := false;
          acc ^ to_s cur)
        else Printf.sprintf "%s, %s" acc (to_s cur)
      in
      List.fold_left cb "[" l ^ "]"
  | Null -> "null"
  | Bool b -> if b then "true" else "false"
  | Map assoc ->
      List.fold_left
        (fun acc cur ->
          let key, value = cur in
          Printf.sprintf "%s\"%s\": %s" acc key (to_s value))
        "{" assoc
      ^ "}"
