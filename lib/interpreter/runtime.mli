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

type process_handle =
  | ProcessInherited of Pid.t
  | ProcessBuffered of {
      pid : Pid.t;
      stdout : Core_unix.File_descr.t;
      stderr : Core_unix.File_descr.t;
    }  (** A reference to a (potentially) running process. *)

type file = { path : string }
type process_result = { code : int; stdout : string; stderr : string }

type t =
  | String of string
  | Bool of bool
  | Num of float
  | Null
  | List of t Array.t
  | HashMap of (t, t) Stdlib.Hashtbl.t
  | Func of function_t
  | Method of t * function_t
  | Prototype of prototype
  | Process of process
  | ProcessHandle of process_handle
  | ProcessResult of process_result
  | File of file
  | FileDescriptor of Core_unix.File_descr.t
  | Directory of string

(* TODO make this hidden *)
and function_t =
  | Native of { cb : t list -> (t, Compiler.Ast.breaking_type * t) Either.t }
  | User of {
      parameters : string list;
      block : Compiler.Optimizer.stmt list;
      identifiers : t Identifiers.t;
      pos : Lexing.position;
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
val process_handle_of_val : t -> process_handle option
val process_result_of_val : t -> process_result option
val func_of_val : t -> function_t option
val method_of_val : t -> (t * function_t) option
val file_of_val : t -> file option
val directory_of_val : t -> string option
val to_class_name : t -> string
val val_of_env : string array -> t
val env_of_val : t -> string array option
