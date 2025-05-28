(* Context *)
type t = { l : (module Sloth_stdlib.StdlibSig); identifiers : Identifiers.t }

let make_ctx m = { l = m; identifiers = Identifiers.create () }
