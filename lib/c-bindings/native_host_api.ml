type c_state
(** This is an opaque type wrapping a C pointer *)

external state_add : c_state -> char -> unit = "state_add"
