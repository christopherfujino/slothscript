(* To flesh out the semantics, this is a pure Ocaml program that directly
   calls the stdlib *)

open Sloth_script
open Sloth_script.Stdlib

let main () =
  let open Runtime in
  let a = Array [ String "One"; Num 2.0 ] in
  InputOutput.print a;
  let m = Map [ ("key", String "value") ] in
  InputOutput.print m;
  let i, _ = Process.run "uname" ["-a"] in
  print_int i;
  InputOutput.print (String "The End.")

let () = main ()
