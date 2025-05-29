type stmt = LetStmt of string * expr | ExprStmt of expr

and expr =
  | Num of float
  | Bool of bool
  | String of string
  | Binary of operator * expr * expr
  | IdRef of string

and operator = Add

let rec num_of_expr expr =
  match expr with
  | Num f -> f
  | _ ->
      let msg = Printf.sprintf "Cast error! %s" (expr_to_str expr) in
      failwith msg

and expr_to_str expr =
  let custom_string_of_float f =
    if Float.is_integer f then Int.of_float f |> string_of_int
    else string_of_float f
  in
  match expr with
  | Num f -> Printf.sprintf "(Num %s)" (custom_string_of_float f)
  | Bool b -> Printf.sprintf "(Bool %s)" (string_of_bool b)
  | Binary (op, left, right) -> (
      match op with
      | Add ->
          Printf.sprintf "(Add %s %s)" (expr_to_str left) (expr_to_str right))
  | String s -> Printf.sprintf "(String \"%s\")" s
  | IdRef i -> Printf.sprintf "(IdRef %s)" i

and stmt_to_str stmt =
  match stmt with
  | LetStmt (name, expr) ->
      Printf.sprintf "(LetStmt %s %s)" name (expr_to_str expr)
  | ExprStmt expr -> Printf.sprintf "(ExprStmt %s)" (expr_to_str expr)
