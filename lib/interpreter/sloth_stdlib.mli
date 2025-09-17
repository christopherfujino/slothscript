open Core

module type StdlibSig = sig
  val print_s : string -> unit
  val file_read_all : string -> string
  val file_write_all : string -> data:string -> unit
  val proc_exec : Runtime.process -> (Runtime.t, string) Result.t
end

module Prod : StdlibSig

module type TestStdlibSig = sig
  include StdlibSig

  val stdout_buffer : string list ref
  val file_system : (string, string) Hashtbl.t
  val proc_expectations : Mock_process.spec option ref
end

module Make_test : functor () -> TestStdlibSig
