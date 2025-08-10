(*
type func_interface = { name : string; arity : int (* Including receiver! *) }

type string_map = t Hashtbl.Make(String).t
and class_interface = { field : t list; methods : string_map }
*)
let globals = [ "print"; "$cwd" ]
