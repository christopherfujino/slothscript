type t

val create : unit -> t
val get : t -> string -> Runtime.t option
