open Core

type t =
  | String of string
  | Bool of bool
  | Num of float
  | List of t list
  (* TODO use dynamic array *)
  | Map of (string * t) list
  (* TODO hashmap *)
  | Null
  | Func of function_t

and function_t =
  | Native of {
      parameters : string list;
      cb : t list -> t;
      identifiers : t Identifiers.t;
    }
  | User of {
      parameters : string list;
      block : Compiler.Optimizer.stmt list;
      identifiers : t Identifiers.t;
    }

let rec num_of_val v =
  match v with
  | Num f -> f
  | _ -> Printf.sprintf "Expected a Num but got %s" (to_s v) |> failwith

and bool_of_val b =
  match b with
  | Bool b' -> b'
  | _ -> Printf.sprintf "Expected a Bool but got %s" (to_s b) |> failwith

and to_s = function
  | String s -> s
  | Num f ->
      if Float.is_integer f then Int.of_float f |> Int.to_string
      else Float.to_string f
  | List l ->
      let is_first = ref true in
      let cb acc cur =
        if !is_first then (
          is_first := false;
          acc ^ to_s cur)
        else Printf.sprintf "%s, %s" acc (to_s cur)
      in
      List.fold_left ~f:cb ~init:"[" l ^ "]"
  | Null -> "null"
  | Bool b -> if b then "true" else "false"
  | Map assoc ->
      List.fold_left
        ~f:(fun acc cur ->
          let key, value = cur in
          Printf.sprintf "%s\"%s\": %s" acc key (to_s value))
        ~init:"{" assoc
      ^ "}"
  | Func _ -> "func(TODO)"
