val parse : Environment.t -> string -> Environment.t * Optimizer.decl list
val debug : Lexing.lexbuf -> Parser.token list -> unit
