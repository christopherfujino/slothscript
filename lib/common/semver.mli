type t = private { major : int; minor : int; patch : int }

val parse : string -> (t, string) Result.t

val create :
  ?major:int -> ?minor:int -> ?patch:int -> unit -> (t, string) Result.t

val is_equal : t -> t -> bool
val to_string : t -> string
