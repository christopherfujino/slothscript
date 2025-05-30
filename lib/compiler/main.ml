open Core

let rec parse line =
  let stmts =
    try Lexing.from_string line |> Parser.prog Lexer.read
    with Parser.Error i ->
      let msg = Printf.sprintf "Parser error (%d)" i in
      (* TODO Interpolate lexer position *)
      failwith msg
  in
  (optimize_stmts [@tailrec]) stmts

and optimize_stmts prog =
  (* TODO Compiler.optimize should compile to IR. *)
  let prog = List.rev prog in
  List.iter prog ~f:optimize_stmt;
  prog

and optimize_stmt stmts =
  let open Ast in
  match stmts with
  | LetStmt (_, expr) -> optimize_expr expr
  | ExprStmt expr -> optimize_expr expr
  | FuncStmt { block; _ } ->
      let _ = optimize_stmts block in
      ()

and optimize_exprs exprs =
  let exprs = List.rev exprs in
  List.iter exprs ~f:optimize_expr

and optimize_expr expr =
  let open Ast in
  match expr with
  | Num _ -> ()
  | Bool _ -> ()
  | String _ -> ()
  | Binary (_, e1, e2) ->
      optimize_expr e1;
      optimize_expr e2
  | FuncInvoc (_, args) -> optimize_exprs args
  | IdRef _ -> ()
