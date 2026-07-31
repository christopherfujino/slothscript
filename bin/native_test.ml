let map_some cb = function Some v -> cb v | None -> failwith "Nope!"
let map_none cb = function None -> cb () | Some _ -> failwith "Nope!"
let map_ok cb = function Ok v -> cb v | Error _ -> failwith "Nope!"
let map_error cb = function Ok _ -> failwith "Nope!" | Error msg -> cb msg

let () =
  Sn.Native.file_read_all "README.md"
  |> map_some (fun _ -> Printf.printf "Read successful.\n");
  Sn.Native.file_read_all "nosuchfile"
  |> map_none (fun () -> Printf.printf "Reading nosuchfile returned None.\n");
  Sn.Native.chdir "/nosuchdir"
  |> map_error (fun msg ->
      Printf.printf "CD-ing to /nosuchdir returned \"%s\"\n" msg);
  Sn.Native.chdir "/"
  |> map_ok (fun () -> Printf.printf "CD-ing to / succeeded.\n")
