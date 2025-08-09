(*
open Core
*)

type func_interface = { name : string; arity : int (* Including receiver! *) }

type t =
  | String
  | Bool
  | Num
  | List
  | HashMap
  | Null
  | Func

let methods = function
  | String -> []
  | Bool -> []
  | Num -> []
  | List -> []
  | HashMap -> []
  | Null -> []
  | Func -> []

let globals = [ { name = "print"; arity = 1 } ]
