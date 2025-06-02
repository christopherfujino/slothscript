type frame

type t = frame list
(** Map-stack. *)

val create : unit -> frame

val set : t -> string -> Runtime.t -> unit
(** Will throw if the identifier already exists on the head of the stack. *)

val get : t -> string -> Runtime.t
(** Will throw if the identifier cannot be found in the stack. *)

val push_new_frame : t -> t
(** Push a new empty frame on the stack *)

val pop : t -> t
(** Pop the top frame off the stack *)
