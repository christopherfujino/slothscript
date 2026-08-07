(*
external _file_read_all : string -> string option = "file_read_all"
external _chdir : string -> (unit, string) Result.t = "_chdir"

module C : Interpreter.Native.Sig = struct
  let file_read_all = _file_read_all
  let chdir = _chdir
end
*)
