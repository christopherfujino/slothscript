type t

val create : unit -> t
val push_empty : t -> t
val get : t -> string -> Runtime.t option
val bind : t -> string -> Runtime.t -> unit option
val reassign : t -> string -> Runtime.t -> unit option
