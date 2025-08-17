open Core

type prototype = { name : string }

type process = {
  cmd : string list;
  mutable stdout : Core_unix.File_descr.t;
  mutable stderr : Core_unix.File_descr.t;
  mutable stdin : Core_unix.File_descr.t;
  mutable pipes_to_collect : Core_unix.File_descr.t list;
  previous : process option;
}

type file = { path : string }
type process_result = { code : int }

type t =
  | String of string
  | Bool of bool
  | Num of float
  | List of t Array.t
  | HashMap of (t, t) Stdlib.Hashtbl.t
  | Null
  | Func of function_t
  | Prototype of prototype
  | Process of process
  | ProcessResult of process_result
  | File of file
  | FileHandle

(* TODO make this hidden *)
and function_t =
  | Native of {
      parameters : string list;
      cb : t list -> (t, string) Result.t;
      identifiers : t Identifiers.t;
    }
  | User of {
      parameters : string list;
      block : Compiler.Optimizer.stmt list;
      identifiers : t Identifiers.t;
    }

type class_t = {
  instance_members : (string, t) Hashtbl.t;
  static_members : (string, t) Hashtbl.t;
}

type class_lookup = (string, class_t) Hashtbl.t

val to_s : t -> string
val num_of_val : t -> float option
val string_of_val : t -> string option
val int_of_val : t -> int option
val bool_of_val : t -> bool option
val list_of_val : t -> t Array.t option
val hashmap_of_val : t -> (t, t) Stdlib.Hashtbl.t option
val process_of_val : t -> process option
val func_of_val : t -> function_t option
val to_class_name : t -> string
