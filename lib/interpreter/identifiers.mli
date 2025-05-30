type t
(** Map-stack. *)

val create : unit -> t
(** Make a t, bruh. *)

val set : t -> string -> Runtime.t -> unit
(** Will throw if the identifier already exists on the head of the stack. *)

val get : t -> string -> Runtime.t
(** Will throw if the identifier cannot be found in the stack. *)

val push : t -> unit
(** Push a new empty frame on the stack *)

val pop : t -> unit
(** Pop the top frame off the stack *)
