(* To flesh out the semantics, this is a pure Ocaml program that directly
   calls the stdlib *)

open Sloth_script
open Sloth_script.Sloth_stdlib

let main () =
  let open Runtime in
  let a = Array [ String "One"; Num 2.0 ] in
  InputOutput.print a;
  let m = Map [ ("key", String "value") ] in
  InputOutput.print m;
  let exit = Process.run "uname" ["-a"] in
  Printf.printf "exited with code %d\n" exit;
  InputOutput.print (String "The End.")

let () = main ()
