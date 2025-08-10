open Core

type t =
  | String of string
  | Bool of bool
  | Num of float
  | List of t Array.t
  | HashMap of (t, t) Stdlib.Hashtbl.t
  | Null
  | Func of function_t

(* TODO make this hidden *)
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

type class_t = { methods : (string, function_t) Hashtbl.t }
type class_lookup = (string, class_t) Hashtbl.t

val to_s : t -> string
val num_of_val : t -> float option
val int_of_val : t -> int option
val bool_of_val : t -> bool option
val to_class_name : t -> string
(*
val invoke_method : class_lookup -> t -> string -> t list -> (t, string) Result.t
*)
