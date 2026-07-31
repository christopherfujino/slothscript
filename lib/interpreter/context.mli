type 'a t

val create : unit -> 'a t
val push_empty : 'a t -> 'a t
val get : 'a t -> string -> 'a option
val bind : 'a t -> string -> 'a -> unit option
val reassign : 'a t -> string -> 'a -> unit option
