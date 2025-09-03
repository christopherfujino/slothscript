type t

val create : string -> t

(* For REPL *)
val update_src : t -> string -> t
val src : t -> string
val push_empty : t -> t
val find : t -> string -> string option
val bind : t -> string -> t option
val find_ctx : t -> string -> string option
val bind_ctx : t -> string -> t option
val populate : t -> t
