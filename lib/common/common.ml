(* This module should have no other Sloth-specific dependencies. *)

exception InternalFailure of string
(** This exception denotes a bug, and should be reported by users. *)

exception LexerError of string
exception ParseError of string

(* fka Optimizer.Failure *)
exception CompileError of string
exception RuntimeError of string

let internal_failure loc =
  raise @@ InternalFailure (Printf.sprintf "Internal failure at %s" loc)

let wrap_error cb =
  try Ok (cb ()) with
  | LexerError msg -> Error msg
  | ParseError msg -> Error msg
  | CompileError msg -> Error msg
  | RuntimeError msg -> Error msg

let debug_mode = false
