type stmt = LetStmt of string * expr | ExprStmt of expr
and expr = Num of float | Bool of bool | Binary of operator * expr * expr
and operator = Add

let rec num_of_expr expr =
  match expr with
  | Num f -> f
  | _ ->
      let msg = Printf.sprintf "Unreachable! %s" (expr_to_str expr) in
      failwith msg

and expr_to_str expr =
  let custom_string_of_float f =
    if Float.is_integer f then Int.of_float f |> string_of_int
    else string_of_float f
  in
  match expr with
  | Num f -> custom_string_of_float f
  | Bool b -> string_of_bool b
  | Binary (op, left, right) -> (
      match op with
      | Add ->
          Printf.sprintf "(Add %s %s)" (expr_to_str left) (expr_to_str right))

and stmt_to_str stmt =
  match stmt with
  | LetStmt (name, expr) ->
      Printf.sprintf "(LetStmt \"%s\" %s)" name (expr_to_str expr)
  | ExprStmt expr -> Printf.sprintf "(ExprStmt %s)" (expr_to_str expr)
