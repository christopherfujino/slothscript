type 'a t

val create : unit -> 'a t

val set : 'a t -> string -> 'a -> 'a t
(** Will throw if the identifier already exists on the head of the stack. *)

val get : 'a t list -> string -> 'a
(** Will throw if the identifier cannot be found in the stack. *)

val reassign : 'a t list -> string -> 'a -> 'a t list
