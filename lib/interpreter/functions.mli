type t

val create : unit -> t
val set : t -> string -> Runtime.function_t -> unit
val get : t -> string -> Runtime.function_t
