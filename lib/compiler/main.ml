open Core

let parse env src =
  let filter, lexbuf = Lexer.bootstrap src in
  let decls =
    try Parser.prog filter lexbuf with
    | Parser.Error i ->
        let pos = lexbuf.lex_start_p in
        let pos_str = Sloth_common.Position.string_of_t lexbuf.lex_start_p in
        let msg = Parser_errors.message i |> String.strip in
        let msg =
          match msg with
          | "<YOUR SYNTAX ERROR MESSAGE HERE>" ->
              Printf.sprintf "[%s] Parser error (code #%d)\n\n%s" pos_str i
                (Sloth_common.Position.summarize pos src)
          | "[UNREACHABLE]" ->
              let msg =
                "This should be unreachable; please file a bug on \
                 https://github.com/christopherfujino/slothscript/issues/new"
              in
              Printf.sprintf "[%s] Parser error (code #%d)\n\n%s\n%s" pos_str i
                (Sloth_common.Position.summarize pos src)
                msg
          | _ ->
              Printf.sprintf "[%s] Parser error (code #%d)\n\n%s\n%s" pos_str i
                (Sloth_common.Position.summarize pos src)
                msg
        in

        raise (Sloth_common.Common.ParseError msg)
    | Lexer.SyntaxError (msg, pos) ->
        let msg =
          Printf.sprintf "[%s] Lexer error\n\n%s\nUnknown token `%s`"
            (Sloth_common.Position.string_of_t pos)
            (Sloth_common.Position.summarize pos src)
            msg
        in
        raise (Sloth_common.Common.LexerError msg)
    | Sloth_common.Common.LexerError msg ->
        let pos = lexbuf.lex_curr_p in
        let msg =
          Printf.sprintf "[%s] Lexer error\n\n%s\n%s"
            (Sloth_common.Position.string_of_t pos)
            (Sloth_common.Position.summarize pos src)
            msg
        in
        raise (Sloth_common.Common.LexerError msg)
  in
  Optimizer.optimize_prog env decls

let debug src =
  let buf = Lexing.from_string src in
  let lex_filter = Lexer.make_lex_filter () in
  let rec inner buf idx r prev =
    let cur = lex_filter buf in
    match cur with
    | Parser.EOF _ ->
        let prev = List.rev prev in
        print_endline "[Lexer debug]";
        List.iter prev ~f:(fun t -> Lexer.token_to_s t |> print_endline);
        ()
    | _ -> (inner [@tailcall]) buf (idx + 1) r (cur :: prev)
  in
  inner buf 0 (ref None) [];
  print_endline ""
