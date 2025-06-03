val interpret_stmt :
  Context.t -> Compiler.Optimizer.stmt -> Context.t * Runtime.t

val interpret_expr : Context.t -> Compiler.Optimizer.expr -> Runtime.t

val interpret_prog :
  Context.t -> Compiler.Optimizer.stmt list -> Context.t * Runtime.t
