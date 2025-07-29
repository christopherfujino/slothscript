val interpret_decl :
  Context.t -> Compiler.Optimizer.decl -> Context.t * Runtime.t

val interpret_stmt :
  Context.t -> Compiler.Optimizer.stmt -> Context.t * Runtime.t

val interpret_expr : Context.t -> Compiler.Optimizer.expr -> Runtime.t

val interpret_prog :
  Context.t -> Compiler.Optimizer.decl list -> Context.t * Runtime.t

val interpret_block : Context.t -> Compiler.Optimizer.stmt list -> Runtime.t
val interpret_cond : Context.t -> Compiler.Optimizer.cond_cont -> Runtime.t
