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
    let pid = Unix.create_process cmd args Unix.stdin Unix.stdout Unix.stderr in
    let pid = Unix.fork () in
    (* 0 is the child *)
    if pid = 0 then exec cmd args else Unix.waitpid [] pid

  let spawn cmd args =
    let arg_len = List.length args in
    let args =
      Array.init (arg_len + 1) (fun idx ->
          if idx = 0 then cmd else List.nth args (idx - 1))
    in
    Unix.create_process cmd args Unix.stdin Unix.stdout Unix.stderr
end

module InputOutput = struct
  let print t = Runtime.to_s t |> print_endline
end
