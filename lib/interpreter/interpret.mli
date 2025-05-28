val interpret_stmt : Context.t -> Compiler.Ast.stmt -> unit

val interpret_expr : Context.t -> Compiler.Ast.expr -> Runtime.t
