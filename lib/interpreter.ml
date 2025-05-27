(* Context *)
type t = { l : (module Sloth_stdlib.StdlibSig) }

let make_ctx _ : t =
  { l = (module Sloth_stdlib.Prod) }

let rec interpret_stmt ctx stmt =
  let open Ast in
  match stmt with
  | LetStmt (_, _) -> failwith "Todo"
  | ExprStmt expr ->
      (* TODO print literal values *)
      let _ = interpret_expr ctx expr in
      ()

and interpret_expr ctx (expr : Ast.expr) =
  let open Ast in
  match expr with
  | Num f -> Runtime.Num f
  | Bool b -> Runtime.Bool b
  | Binary (op, lhs, rhs) -> (
      match op with
      | Add ->
          let left_val = Runtime.num_of_val (interpret_expr ctx lhs) in
          let right_val = Runtime.num_of_val (interpret_expr ctx rhs) in
          Runtime.Num (left_val +. right_val))
