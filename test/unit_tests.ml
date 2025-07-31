open OUnit2
open Core

let debug_src_print src =
  String.iter src ~f:(fun c ->
      match c with '\n' -> Printf.printf "\\n\n" | _ -> Printf.printf "%c" c)

let pretty =
  [
    ( "escapes string literals with whitespace",
      fun _ ->
        let src = {|


func f() {
  42;
}

print("The answer is: ${f()}.");|} in

        let buf = Lexing.from_string src in
        Compiler.Main.debug buf [];
        debug_src_print src;
        let env =
          Compiler.Environment.create () |> Compiler.Stdlib_stubs.populate
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

let get () =
  let f =
   fun tuple ->
    let name, callback = tuple in
    name >:: callback
  in
  [ "pretty" >::: List.map pretty ~f ]
