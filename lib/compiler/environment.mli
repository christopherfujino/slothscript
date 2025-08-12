type t

val create : string -> t
val src : t -> string
val push_empty : t -> t
val find : t -> string -> string option
val has_proto : t -> string -> bool
val bind : t -> string -> t option
val populate : t -> t
