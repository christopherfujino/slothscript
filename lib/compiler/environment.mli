type t = { previous : t option; values : string list; src : string }

val create : string -> t
val push_empty : t -> t
val find : t -> string -> string option
val bind : t -> string -> t option
val populate : t -> t
