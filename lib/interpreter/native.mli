open Core

module type Sig = Native_sig.Sig

module Prod : Sig

module type TestSig = sig
  include Sig

  type fs_entity = File of string ref * Core_unix.File_descr.t | Directory

  val get_stdout : unit -> string
  val path_to_entity : (string, fs_entity) Hashtbl.t
  val proc_expectations : Mock_process.spec option ref
end

val make_test : Mock_process.spec -> (module TestSig)
