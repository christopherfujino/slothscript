(* Context *)
type t = {
  l : (module Sloth_stdlib.StdlibSig);
  identifiers : Runtime.t Identifiers.t;
  src : string;
}

let make_ctx m src =
  let module M = (val m : Sloth_stdlib.StdlibSig) in
  let identifiers = Identifiers.create () in
  let unit_opt =
    Identifiers.bind identifiers "print"
      (Runtime.Func
         (Native
            {
              parameters = [ "value" ];
              cb =
                (fun args ->
                  if List.length args != 1 then
                    failwith
                      "You passed the wrong number of arguments to print()";
                  let arg = List.hd args in
                  M.InputOutput.print arg;
                  Runtime.Null);
              identifiers;
            }))
  in
  let open Core in
  Option.value_exn unit_opt;
  { l = m; identifiers; src }
