open Core

type processMode =
  | BlockInherit (* proc! -> null *)
  | ForkBuffer (* proc& -> ProcessHandle *)
  | BlockBuffer (* proc&! -> ProcessResult *)

module type Sig = sig
  val print_s : string -> unit
  val file_read_all : string -> string
  val write : Core_unix.File_descr.t -> data:string -> (unit, string) Result.t
  val wait : Runtime.process_handle -> (Runtime.t, string) Result.t

  val proc_exec :
    mode:processMode ->
    Runtime.process ->
    string array ->
    (Runtime.t, string) Result.t

  val chdir : string -> unit
  val directory_exists : string -> bool
  val mkdir : string -> unit
end

module Prod : Sig

module type TestSig = sig
  include Sig

  type fs_entity = File of string | Directory

  val stdout_buffer : string list ref
  val path_to_fd : (string, int) Hashtbl.t
  val fd_to_entity : (int, fs_entity) Hashtbl.t
  val proc_expectations : Mock_process.spec option ref
end

val make_test : Mock_process.spec -> (module TestSig)
