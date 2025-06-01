(* Context *)
type t = {
  l : (module Sloth_stdlib.StdlibSig);
  identifiers : Identifiers.t;
  functions : Functions.t;
}

let make_ctx m =
  let fs = Functions.create () in
  let module M = (val m : Sloth_stdlib.StdlibSig) in
  Functions.set fs "print"
    (Native
       {
         parameters = [ "value" ];
         cb =
           (fun args ->
             if List.length args != 1 then
               failwith "You passed the wrong number of arguments to print()";
             let arg = List.hd args in
             M.InputOutput.print arg;
             Runtime.Null);
       });
  { l = m; identifiers = [ Identifiers.create () ]; functions = fs }
