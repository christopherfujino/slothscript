open OUnit2
open Core

let pretty =
  [
    ( "escapes string literals with whitespace",
      fun _ ->
        let src = {|


func f() {
  42;
}

print("The answer is: ${f()}.")|} in

        let env =
          Compiler.Environment.create src |> Compiler.Environment.populate
        in
        let _, ast = Compiler.Main.parse env src in
        let ast_str = Compiler.Optimizer.prog_to_str ast in
        let pretty = Printer.sexp_formatter ast_str in
        let double_quote_count =
          String.fold pretty ~init:0 ~f:(fun i c ->
              if Char.( = ) c '"' then i + 1 else i)
        in
        assert_equal double_quote_count 2 );
  ]

let escape () =
  let open Interpreter.Common in
  let pp_diff formatter tuple =
    let left, right = tuple in
    let left = List.to_string ~f:Fun.id left in
    let right = List.to_string ~f:Fun.id right in
    Format.fprintf formatter "Actual: \"%s\"\nExpected: \"%s\"" left right
  in
  [
    ( "with no spaces, returns a single element",
      fun _ ->
        let input = "thisISaLongSTRING_without.anySPACES" in
        let output = shell_like_escape input in
        assert_equal ~pp_diff output [ input ] );
    ( "multiple elements output in the right order",
      fun _ ->
        let output = shell_like_escape "two words" in
        assert_equal ~pp_diff output [ "two"; "words" ] );
  ]

let get () =
  let f =
   fun tuple ->
    let name, callback = tuple in
    name >:: callback
  in
  [
    "shell_like_escape" >::: List.map (escape ()) ~f;
    "pretty" >::: List.map pretty ~f;
  ]
