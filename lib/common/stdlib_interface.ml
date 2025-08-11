(*
type func_interface = { name : string; arity : int (* Including receiver! *) }

type string_map = t Hashtbl.Make(String).t
and class_interface = { field : t list; methods : string_map }
*)

type t = Value | Class of { properties : string list; methods : string list }

let globals =
  [
    ("print", Value);
    ("assert", Value);
    ("$cwd", Value);
    ( "Number",
      Class
        {
          properties = [];
          methods = [ "+"; "-"; "/"; "*"; "<"; ">"; "<="; ">=" ];
        } );
  ]
