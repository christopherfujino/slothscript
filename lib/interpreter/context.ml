module Identifiers = struct
  type t = (string, Runtime.t) Hashtbl.t list ref

  let create () = ref [ Hashtbl.create 8 ]

  let set (ids : t) id v =
    let tbl = List.hd !ids in
    let maybe_val = Hashtbl.find_opt tbl id in
    match maybe_val with
    | Some _ ->
        let msg = Printf.sprintf "cannot rebind name %s" id in
        failwith msg
    | None -> Hashtbl.add tbl id v
end

(* Context *)
type t = { l : (module Sloth_stdlib.StdlibSig); identifiers : Identifiers.t }

let make_ctx m = { l = m; identifiers = Identifiers.create () }

let rec interpret_stmt ctx stmt =
  let open Compiler.Ast in
  match stmt with
  | LetStmt (id, e) ->
      let v = interpret_expr ctx e in
      Identifiers.set ctx.identifiers id v
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
