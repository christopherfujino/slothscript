type t

type function_t =
  | Native of {
      parameters : string list;
      cb : Runtime.t list -> Runtime.t;
      identifiers : t list;
    }
  | User of {
      parameters : string list;
      block : Compiler.Optimizer.stmt list;
      identifiers : t list;
    }

val create : unit -> t

val set : t list -> string -> Runtime.t -> unit
(** Will throw if the identifier already exists on the head of the stack. *)

val get : t list -> string -> Runtime.t
(** Will throw if the identifier cannot be found in the stack. *)
