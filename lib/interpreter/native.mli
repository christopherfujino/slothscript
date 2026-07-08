open Core

type processMode =
  | BlockInherit (* proc! -> null *)
  | ForkInherit (* ? -> ProcessHandle *)
  | ForkBuffer (* proc& -> ProcessHandle *)
  | BlockBuffer (* proc&! -> ProcessResult *)

module type Sig = sig
  val file_read_all : string -> string
  val fd_read_all : Core_unix.File_descr.t -> (Runtime.t, string) Result.t
  val fd_write_all : Core_unix.File_descr.t -> string -> (unit, string) Result.t
  val write : Core_unix.File_descr.t -> data:string -> (unit, string) Result.t
  val read : Core_unix.File_descr.t -> (Runtime.t, string) Result.t
  val wait : Runtime.process_handle -> (Runtime.t, string) Result.t
  val pipe : unit -> Core_unix.File_descr.t * Core_unix.File_descr.t

  val open_file :
    mode:Core_unix.open_flag list ->
    string ->
    (Core_unix.File_descr.t, string) Result.t

  val close : Core_unix.File_descr.t -> (unit, string) Result.t

  val proc_exec :
    mode:processMode ->
    Runtime.process ->
    string array ->
    (Runtime.t, string) Result.t

  val chdir : string -> (unit, string) Result.t
  val directory_exists : string -> bool
  val file_exists : string -> bool
  val mkdir : string -> unit
end

module Prod : Sig

module type TestSig = sig
  include Sig

  type fs_entity = File of string ref * Core_unix.File_descr.t | Directory

  val get_stdout : unit -> string
  val path_to_entity : (string, fs_entity) Hashtbl.t
  val proc_expectations : Mock_process.spec option ref
end

val make_test : Mock_process.spec -> (module TestSig)
