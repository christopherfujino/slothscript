open OUnit2
open Sloth_script

let parser_tests =
  "parser"
  >::: [
         ( "todo" >:: fun _ ->
           let _ = Lexing.from_string "11" |> Parser.prog Lexer.read in
           () );
       ]

let () = run_test_tt_main parser_tests
