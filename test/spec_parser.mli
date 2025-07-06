type state = NotParsing | Parsing of string * string list

val deserialize : string -> Common.test_spec
val serialize : string -> Common.test_spec -> unit
