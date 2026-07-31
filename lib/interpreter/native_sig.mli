open Core

(* This is its own, interface-only module to work around:
  https://discuss.ocaml.org/t/why-do-i-need-to-repeat-type-declarations-between-interfaces-and-implementations-or-how-do-i-get-around-this/3350 *)

type processMode =
  | BlockInherit (* proc! -> null *)
  | ForkInherit (* ? -> ProcessHandle *)
  | ForkBuffer (* proc& -> ProcessHandle *)
  | BlockBuffer (* proc&! -> ProcessResult *)

module type Sig = sig
  (* TODO we should never be returning Runtime.t, but more specific types *)

  val file_read_all : string -> string
  val fd_read_all : Core_unix.File_descr.t -> (Runtime.t, string) Result.t
  val fd_write_all : Core_unix.File_descr.t -> string -> (unit, string) Result.t

  (* TODO does this type of `data` need to be more generic to support binary? *)
  val write : Core_unix.File_descr.t -> data:string -> (unit, string) Result.t
  val read : Core_unix.File_descr.t -> (Runtime.t, string) Result.t
  val wait : Runtime.process_handle -> (Runtime.t, string) Result.t

  val pipe : unit -> Core_unix.File_descr.t * Core_unix.File_descr.t
  (** unit -> read * write *)

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
  val realpath : string -> string
end
