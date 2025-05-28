type t
(** Map-stack. *)

val create : unit -> t
(** Make a t, bruh. *)

val set : t -> string -> Runtime.t -> unit
(** Will throw if the identifier already exists on the head of the stack. *)
