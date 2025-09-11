open Core

module type StdlibSig = sig
  val print_s : string -> unit
  val file_read_all : string -> string
  val file_write_all : string -> data:string -> unit
end

module Prod : StdlibSig = struct
  let print_s = Printf.printf "%s%!"
  let file_read_all = In_channel.read_all
  let file_write_all = Out_channel.write_all
end

module type TestStdlibSig = sig
  val stdout_buffer : string list ref
  val file_system : (string, string) Hashtbl.t

  include StdlibSig
end

module Make_test () : TestStdlibSig = struct
  let stdout_buffer : string list ref = ref []
  let file_system = Hashtbl.create (module String)

  let file_write_all path ~data =
    (* TODO allow over-writing *)
    Hashtbl.add_exn file_system ~key:path ~data

  let file_read_all path =
    (* TODO: resolve relative paths *)
    Hashtbl.find file_system path
    |> Option.value_exn ~message:(Printf.sprintf "Error no entity \"%s\"" path)

  let print_s s = stdout_buffer := s :: !stdout_buffer
end
