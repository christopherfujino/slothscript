(* This assumes STDLIB option & result
   Check for correctness if switching to Core! *)

external file_read_all : string -> string option = "file_read_all"
external chdir : string -> (string, string) Result.t = "_chdir"
(*
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

  val directory_exists : string -> bool
  val file_exists : string -> bool
  val mkdir : string -> unit
*)
