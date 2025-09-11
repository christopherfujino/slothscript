open Core

module type StdlibSig = sig
  val print_s : string -> unit
  val file_read_all : string -> string
  val file_write_all : string -> data:string -> unit
end

module Prod : StdlibSig

module type TestStdlibSig = sig
  include StdlibSig

  val stdout_buffer : string list ref
  val file_system : (string, string) Hashtbl.t
end

module Make_test : functor () -> TestStdlibSig
