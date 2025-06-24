type t

val create : unit -> t
val bind : t -> string -> t
val push_empty : t -> t
val find : t -> string -> string option
