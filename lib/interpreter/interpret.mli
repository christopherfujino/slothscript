open Core

val interpret_decl :
  Globals.t ->
  Compiler.Optimizer.decl ->
  Globals.t * (Runtime.t, Runtime.breaking_type) Either.t

val interpret_stmt :
  Globals.t ->
  Compiler.Optimizer.stmt ->
  Globals.t * (Runtime.t, Runtime.breaking_type) Either.t

val interpret_expr :
  Globals.t ->
  Compiler.Optimizer.expr ->
  Globals.t * (Runtime.t, Runtime.breaking_type) Either.t

val interpret_prog :
  Globals.t ->
  Compiler.Optimizer.decl list ->
  Globals.t * (Runtime.t, Runtime.breaking_type) Either.t
