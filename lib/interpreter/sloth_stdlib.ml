module Process = struct
  type t = {
    state : state;
    mutable stdin : Unix.file_descr;
    mutable stdout : Unix.file_descr;
    mutable stderr : Unix.file_descr;
  }

  and state = Unstarted | Running | Finished

  let ( |>> ) left right =
    let read, write = Unix.pipe () in
    left.stdout <- write;
    right.stdin <- read;
    right

  let exec cmd args = Unix.execvp cmd args
  (* p in execvp is use path. This will never return. *)

  let run cmd args =
    let arg_len = List.length args in
    let args =
      Array.init (arg_len + 1) (fun idx ->
          if idx = 0 then cmd else List.nth args (idx - 1))
    in
    let pid = Unix.create_process cmd args Unix.stdin Unix.stdout Unix.stderr in
    let _, status = Unix.waitpid [] pid in
    match status with
    | WEXITED exit -> exit
    | WSIGNALED signal -> -signal
    | WSTOPPED signal -> -signal

  let spawn cmd args =
    let arg_len = List.length args in
    let args =
      Array.init (arg_len + 1) (fun idx ->
          if idx = 0 then cmd else List.nth args (idx - 1))
    in
    Unix.create_process cmd args Unix.stdin Unix.stdout Unix.stderr
end

module type StdlibSig = sig
  module InputOutput : sig
    val print_s : string -> unit
    val print : Runtime.t -> unit
  end
end

module type StdlibInputSig = sig
  val print_s : string -> unit
end

module Make (T : StdlibInputSig) : StdlibSig = struct
  module InputOutput = struct
    let print_s = T.print_s
    let print t = Runtime.to_s t ^ "\n" |> print_s
  end
end

module Prod = Make (struct
  let print_s = print_string
end)

module Test = struct
  let stdout_buffer : string list ref = ref []

  include Make (struct
    let print_s s = stdout_buffer := s :: !stdout_buffer
  end)
end
