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

print("The answer is: ${f()}.");|} in
        let _, ast =
          (Compiler.Environment.create ()
          |> Compiler.Stdlib_stubs.populate |> Compiler.Main.parse)
          @@ src
        in
        let ast_str = Compiler.Optimizer.prog_to_str ast in
        let pretty = Printer.sexp_formatter ast_str in
        let double_quote_count =
          String.fold pretty ~init:0 ~f:(fun i c ->
              if Char.( = ) c '"' then i + 1 else i)
        in
        assert_equal double_quote_count 2 );
  ]

let get () =
  let f =
   fun tuple ->
    let name, callback = tuple in
    name >:: callback
  in
  [ "pretty" >::: List.map pretty ~f ]
