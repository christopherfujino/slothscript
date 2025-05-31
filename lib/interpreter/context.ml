(* Context *)
type t = {
  l : (module Sloth_stdlib.StdlibSig);
  identifiers : Identifiers.t;
  functions : Functions.t;
}

let make_ctx m =
  {
    l = m;
    identifiers = [ Identifiers.create () ];
    functions = Functions.create ();
  }
