let () =
  Sn.Native.file_read_all "README.md"
  |> Printf.printf "Sn.Native.file_read_all returned \"%s\"\n"
