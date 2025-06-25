open Core

let parse line =
  let decls =
    try Lexing.from_string line |> Parser.prog Lexer.read
    with Parser.Error i ->
      let msg = Printf.sprintf "Parser error (%d)" i in
      (* TODO Interpolate lexer position *)
      raise (Common.ParserFailure msg)
  in
  Optimizer.optimize_prog decls
