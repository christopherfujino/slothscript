let rec parse line =
  let stmts =
    try Lexing.from_string line |> Parser.prog Lexer.read
    with Parser.Error i ->
      let msg = Printf.sprintf "Parser error (%d)" i in
      (* TODO Interpolate lexer position *)
      failwith msg
  in
  (optimize [@tailrec]) stmts

and optimize prog =
  (* TODO Compiler.optimize should compile to IR. *)
  List.rev prog
