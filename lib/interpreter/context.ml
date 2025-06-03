(* Context *)
type t = {
  l : (module Sloth_stdlib.StdlibSig);
  identifiers : Runtime.t Identifiers.t list;
}

let make_ctx m =
  let module M = (val m : Sloth_stdlib.StdlibSig) in
  let identifiers = [ Identifiers.create () ] in
  Identifiers.set identifiers "print"
    (Runtime.Func (Native
       {
         parameters = [ "value" ];
         cb =
           (fun args ->
             if List.length args != 1 then
               failwith "You passed the wrong number of arguments to print()";
             let arg = List.hd args in
             M.InputOutput.print arg;
             Runtime.Null);
         identifiers;
       }));
  { l = m; identifiers }
