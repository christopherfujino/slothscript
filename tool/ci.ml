module Target = struct
  type t = {
    cmd : string list;
    root : string;
    key : string;
    (* sha256 *)
    hasher_opt : (unit -> string) option;
  }

  type result = Ok | Error of string list

  let bind res func =
    let res2 = func () in
    match res2 with
    | Ok -> res
    | Error new_errors -> (
        match res with
        | Ok -> res2
        | Error old_errors -> Error (old_errors @ new_errors))

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
    if is_fresh then (
      Printf.printf "Target %s is up to date\n%!" target.key;
      Ok)
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
          if code = 0 then Ok
          else
            let cmd_str_opt =
              List.fold_left
                (fun acc cur ->
                  match acc with
                  | None -> Some cur
                  | Some prev -> Some (Printf.sprintf "%s %s" prev cur))
                None cmd
            in
            let cmd_str = Option.get cmd_str_opt in
            Error [ Printf.sprintf "`%s` failed with code %d" cmd_str code ]
      | WSIGNALED _ -> Error [ "Subprocess failed with a signal" ]
      | WSTOPPED _ -> Error [ "Subprocess stopped" ])
end

let () =
  let ( let* ) = Target.bind in
  let root =
    Unix.open_process_in "git rev-parse --show-toplevel"
    |> In_channel.input_all |> String.trim
  in
  let create = Target.create root in
  let res =
    let* () = create "yolos" None |> Target.run in
    let* () =
      create "get" (Some "cat sloth_script.opam dune-project | sha256sum")
      |> Target.run
    in
    let* () = create "build" None |> Target.run in
    let* () = create "check-format" None |> Target.run in
    let* () = create "test" None |> Target.run in
    let* () = create "integration-tests" None |> Target.run in
    create "docs" None |> Target.run
  in
  match res with
  | Ok -> print_endline "CI suite successful!"
  | Error errs ->
      print_endline "CI suite failed with the following errors:";
      List.iter (fun msg -> Printf.printf "-> %s\n" msg) errs
