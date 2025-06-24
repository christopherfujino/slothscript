type 'a t

val create : unit -> 'a t
val push_empty : 'a t -> 'a t

val get : 'a t -> string -> 'a
(** Will throw if the identifier cannot be found in the stack. *)

val bind : 'a t -> string -> 'a -> unit
(** Will throw if the identifier already exists on the head of the stack. *)

val reassign : 'a t -> string -> 'a -> unit
