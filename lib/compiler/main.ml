let parse_line line =
  try Lexing.from_string line |> Parser.prog Lexer.read
  with Parser.Error i ->
    let msg = Printf.sprintf "Parser error (%d)" i in
    (* TODO Interpolate lexer position *)
    failwith msg
