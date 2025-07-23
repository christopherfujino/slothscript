open Core

module type pType = sig end

module MakeType (P : pType) = struct
  module type T = sig
    type t = P

    val to_s : t -> string
  end
end

module type tType = sig
  type 'a t

  val to_s : 'a t -> string
end

module type pType = sig end

module Make (T : tType) (P : pType) : tType = struct
  (*
    type P t
  *)

  let to_s = T.to_s
end

module Num = Make (struct
  type t = Float.t

  let to_s f =
    if Float.is_integer f then Int.of_float f |> Int.to_string
    else Float.to_string f
end)

module SlothString = Make (struct
  type t = String.t

  let to_s = Fun.id
end)

module SlothBool = Make (struct
  type t = Bool.t

  let to_s = Bool.to_string
end)

module SlothList = Make (struct
  type t = mType list

  let to_s _ = "TODO"
end)

(*
  | String of string
  | Bool of bool
  | Num of float
  | Null
  | List of t list
  | HashMap of (t, t) Stdlib.Hashtbl.t
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
      List.fold_left ~f:cb ~init:"[" l ^ "]"
  | Null -> "null"
  | Bool b -> if b then "true" else "false"
  | HashMap tbl ->
      Stdlib.Hashtbl.fold
        (fun key data acc ->
          Printf.sprintf "%s%s: %s," acc (to_s key) (to_s data))
        tbl "{"
      ^ "}"
  | Func _ -> "func(TODO)"

let num_of_val v =
  match v with
  | Num f -> f
  | _ -> Printf.sprintf "Expected a Num but got %s" (to_s v) |> failwith

let bool_of_val b =
  match b with
  | Bool b' -> b'
  | _ -> Printf.sprintf "Expected a Bool but got %s" (to_s b) |> failwith

*)
