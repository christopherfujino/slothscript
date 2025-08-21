open Core

val interpret_decl :
  Context.t -> Compiler.Optimizer.decl -> Context.t * Runtime.t

val interpret_stmt :
  Context.t ->
  Compiler.Optimizer.stmt ->
  Context.t * Compiler.Ast.breaking_type option * Runtime.t

val interpret_expr :
  Context.t ->
  Compiler.Optimizer.expr ->
  (Runtime.t, Compiler.Ast.breaking_type * Runtime.t) Either.t

val interpret_prog :
  Context.t -> Compiler.Optimizer.decl list -> Context.t * Runtime.t
