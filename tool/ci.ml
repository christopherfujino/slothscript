let run cmd =
  let process = match cmd with hd :: _ -> hd | _ -> failwith "usage error!" in
  let pid =
    Unix.create_process process (Array.of_list cmd) Unix.stdin Unix.stdout
      Unix.stderr
  in
  let _, status = Unix.waitpid [] pid in
  match status with
  | WEXITED code -> if code = 0 then () else failwith "Whoops!"
  | WSIGNALED _ -> failwith "Subprocess failed with a signal"
  | WSTOPPED _ -> failwith "Subprocess stopped"

let () =
  let root =
    Unix.open_process_in "git rev-parse --show-toplevel"
    |> In_channel.input_all |> String.trim
  in
  run [ "make"; "-C"; root; "get" ];
  run [ "make"; "-C"; root; "build" ];
  run [ "make"; "-C"; root; "test" ];
