type 'a t

val create : unit -> 'a t

val set : 'a t list -> string -> 'a -> unit
(** Will throw if the identifier already exists on the head of the stack. *)

val get : 'a t list -> string -> 'a
(** Will throw if the identifier cannot be found in the stack. *)
