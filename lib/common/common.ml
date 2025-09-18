(* This module should have no other Sloth-specific dependencies. *)

exception InternalFailure of string
(** This exception denotes a bug, and should be reported by users. *)

exception ParseError of string

(* fka Optimizer.Failure *)
exception CompileError of string
exception RuntimeError of string

let internal_failure loc =
  raise @@ InternalFailure (Printf.sprintf "Internal failure at %s" loc)

let wrap_error cb =
  try Ok (cb ()) with
  | ParseError msg ->
      let msg = Printf.sprintf "ParseError:\n\n%s" msg in
      Error msg
  | CompileError msg ->
      let msg = Printf.sprintf "CompileError:\n\n%s" msg in
      Error msg
  | RuntimeError msg ->
      let msg = Printf.sprintf "RuntimeError:\n\n%s" msg in
      Error msg
