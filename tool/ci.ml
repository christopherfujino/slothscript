module Target = struct
  type t = {
    cmd : string list;
    root : string;
    key : string;
    (* sha256 *)
    hasher_opt : (unit -> string) option;
  }

  let create root make_target hasher_cmd_opt =
    {
      cmd = [ "make"; "-C"; root; make_target ];
      root;
      key = make_target;
      hasher_opt =
        Option.map
          (fun cmd ->
            fun () -> Unix.open_process_in cmd |> In_channel.input_all)
          hasher_cmd_opt;
    }

  let ensure_dir path =
    try
      let _ = Unix.stat path in
      ()
    with Unix.Unix_error _ -> Unix.mkdir path 0o755

  let is_up_to_date target hash =
    let ignore_dir = Printf.sprintf "%s/ignore" target.root in
    ensure_dir ignore_dir;
    let f = Printf.sprintf "%s/%s.stamp" ignore_dir target.key in
    let write () =
      let chan = Out_channel.open_text f in
      Out_channel.output_string chan hash;
      Out_channel.flush chan;
      Out_channel.close chan
    in
    try
      let _ = Unix.stat f in
      (* open_text does newline conversion *)
      let chan = In_channel.open_text f in
      let stored_hash = In_channel.input_all chan in
      let is_fresh = hash = stored_hash in
      if not is_fresh then Printf.printf "Cache is dirty\n";
      write ();
      is_fresh
    with Unix.Unix_error _ ->
      write ();
      false

  let run target =
    let cmd = target.cmd in
    let process =
      match cmd with hd :: _ -> hd | _ -> failwith "usage error!"
    in
    let is_fresh =
      match target.hasher_opt with
      | None -> false
      | Some hasher ->
          let hash = hasher () in
          is_up_to_date target hash
    in
    if is_fresh then Printf.printf "Target %s is up to date\n%!" target.key
    else (
      Printf.printf "Executing%s\n%!"
        (List.fold_left (fun acc cur -> Printf.sprintf "%s %s" acc cur) "" cmd);
      let pid =
        Unix.create_process process (Array.of_list cmd) Unix.stdin Unix.stdout
          Unix.stderr
      in
      let _, status = Unix.waitpid [] pid in
      match status with
      | WEXITED code ->
          if code = 0 then ()
          else
            let cmd_str =
              List.fold_left
                (fun acc cur -> Printf.sprintf "%s, %s" acc cur)
                "" cmd
            in
            let msg =
              Printf.sprintf "Invocation [%s] failed with code %d" cmd_str code
            in
            failwith msg
      | WSIGNALED _ -> failwith "Subprocess failed with a signal"
      | WSTOPPED _ -> failwith "Subprocess stopped")
end

let () =
  let root =
    Unix.open_process_in "git rev-parse --show-toplevel"
    |> In_channel.input_all |> String.trim
  in
  let create = Target.create root in
  create "get" (Some "cat sloth_script.opam dune-project | sha256sum")
  |> Target.run;
  create "build" None |> Target.run;
  create "test" None |> Target.run
