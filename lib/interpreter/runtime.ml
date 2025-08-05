open Core

type t =
  | String of string
  | Bool of bool
  | Num of float
  | List of t Array.t
  | HashMap of (t, t) Stdlib.Hashtbl.t
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

let sexp_of_t _ = failwith "TODO"
let compare _ = failwith "TODO"
let hash _ = failwith "TODO"

let rec to_s = function
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
      Array.fold ~f:cb ~init:"[" l ^ "]"
  | Null -> "null"
  | Bool b -> if b then "true" else "false"
  | HashMap tbl ->
      Stdlib.Hashtbl.fold
        (fun key data acc ->
          Printf.sprintf "%s%s: %s," acc (to_s key) (to_s data))
        tbl "{"
      ^ "}"
  | Func _ -> "func(TODO)"

let num_of_val v pos =
  match v with
  | Num f -> f
  | _ ->
      Printf.sprintf "Expected a Num but got %s at %s" (to_s v)
        (Sloth_common.Position.string_of_t pos)
      |> failwith

let int_of_val v pos =
  let f = num_of_val v pos in
  if Float.is_integer f then Int.of_float f
  else Printf.sprintf "The Num value %f is not an integer" f |> Common.failure pos

let bool_of_val b =
  match b with
  | Bool b' -> b'
  | _ -> Printf.sprintf "Expected a Bool but got %s" (to_s b) |> failwith
