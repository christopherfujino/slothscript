(* Context *)
type t = { l : (module Sloth_stdlib.StdlibSig) }

let make_prod_ctx () : t = { l = (module Sloth_stdlib.Prod) }

let rec interpret_stmt ctx stmt =
  let open Compiler.Ast in
  match stmt with
  | LetStmt (_, _) -> (* TODO *) failwith "Todo"
  | ExprStmt expr ->
      let v = interpret_expr ctx expr in
      let module L = (val ctx.l) in
      L.InputOutput.print v;
      ()

and interpret_expr ctx (expr : Compiler.Ast.expr) =
  let open Compiler.Ast in
  match expr with
  | Num f -> Runtime.Num f
  | Bool b -> Runtime.Bool b
  | Binary (op, lhs, rhs) -> (
      match op with
      | Add ->
          let left_val = Runtime.num_of_val (interpret_expr ctx lhs) in
          let right_val = Runtime.num_of_val (interpret_expr ctx rhs) in
          Runtime.Num (left_val +. right_val))
