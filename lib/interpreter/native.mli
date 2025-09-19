open Core

module type Sig = sig
  val print_s : string -> unit
  val file_read_all : string -> string
  val file_write_all : string -> data:string -> unit

  val proc_exec :
    Runtime.process -> string array -> (Runtime.t, string) Result.t

  val chdir : string -> unit
  val directory_exists : string -> bool
  val mkdir : string -> unit
end

module Prod : Sig

module type TestSig = sig
  include Sig

  type fs_entity = File of string | Directory

  val stdout_buffer : string list ref
  val file_system : (string, fs_entity) Hashtbl.t
  val proc_expectations : Mock_process.spec option ref
end

val make_test : Mock_process.spec -> (module TestSig)
