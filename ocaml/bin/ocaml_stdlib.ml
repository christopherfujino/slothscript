(* To flesh out the semantics, this is a pure Ocaml program that directly
   calls the stdlib *)

open Sloth_script
open Sloth_script.Stdlib

let main () =
  let a = Types.Array [Types.String "One"; Types.Num 2.0] in
  InputOutput.print a;
  (* Process.spawn "sleep" ["100"];
  print_endline "post spawn" *)
  InputOutput.print (Types.String "The End.")

let () = main ()
