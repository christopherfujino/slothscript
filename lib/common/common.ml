(* This module should have no other Sloth-specific dependencies. *)

exception InternalFailure of string
(** This exception denotes a bug, and should be reported by users. *)

exception LexerError of string
exception ParseError of string

(* fka Optimizer.Failure *)
exception CompileError of string
exception RuntimeError of string

let internal_failure msg =
  raise
  @@ InternalFailure
       (Printf.sprintf
          "%s\n\n\
           Please file a bug at \
           https://github.com/christopherfujino/slothscript/issues/new"
          msg)

let wrap_error cb =
  try Ok (cb ()) with
  | LexerError msg -> Error msg
  | ParseError msg -> Error msg
  | CompileError msg -> Error msg
  | RuntimeError msg -> Error msg

let option_value opt ~message =
  match opt with Some v -> v | None -> raise @@ InternalFailure message

let debug_mode = false

(* See BUFSIZ in stdio.h = 8192; although apparently OCaml's IO buffers are 65536? *)
let bufsiz = 65536
