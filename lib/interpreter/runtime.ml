type t =
  | String of string
  | Bool of bool
  | Num of float
  | Array of t list
  (* TODO use dynamic array *)
  | Map of (string * t) list
  (* TODO hashmap *)
  | Null

let num_of_val v = match v with Num f -> f | _ -> failwith "Cast error!"

let rec to_s = function
  | String s -> Printf.sprintf "\"%s\"" s
  | Num f ->
      if Float.is_integer f then Int.of_float f |> Int.to_string
      else Float.to_string f
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
