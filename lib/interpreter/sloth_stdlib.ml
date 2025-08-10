open Core

module type StdlibSig = sig
  val print_s : string -> unit
end

module type StdlibInputSig = sig
  val print_s : string -> unit
end

module Make (T : StdlibInputSig) : StdlibSig = struct
  let print_s = T.print_s
end

module Prod = Make (struct
  let print_s = print_string
end)

module type TestStdlibSig = sig
  val stdout_buffer : string list ref

  include StdlibSig
end

module Make_test () : TestStdlibSig = struct
  let stdout_buffer : string list ref = ref []

  include Make (struct
    let print_s s = stdout_buffer := s :: !stdout_buffer
  end)
end
