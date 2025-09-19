open Core

module type Sig = sig
  val print_s : string -> unit
  val file_read_all : string -> string
  val file_write_all : string -> data:string -> unit

  val proc_exec :
    Runtime.process -> string array -> (Runtime.t, string) Result.t

  val chdir : string -> unit
  val directory_exists : string -> bool
  val mkdir : string -> unit
end

module Prod : Sig = struct
  let print_s = Printf.printf "%s%!"
  let file_read_all = In_channel.read_all
  let file_write_all = Out_channel.write_all
  let chdir = Core_unix.chdir
  let mkdir = Core_unix.mkdir ~perm:0o775

  let directory_exists path =
    match Sys_unix.is_directory ~follow_symlinks:true path with
    | `Yes -> true
    | `No -> false
    | `Unknown -> false (* TODO? *)

  let proc_exec (proc : Runtime.process) env =
    let read_stdout, write_stdout = Core_unix.pipe ~close_on_exec:true () in
    let read_stderr, write_stderr = Core_unix.pipe ~close_on_exec:true () in
    let original_stdout = proc.stdout in
    let original_stderr = proc.stderr in
    proc.stdout <- write_stdout;
    proc.stderr <- write_stderr;
    let rec get_pids proc =
      let prev_pids =
        match Runtime.(proc.previous) with
        | Some prev -> get_pids prev
        | None -> []
      in
      let prog = List.hd_exn Runtime.(proc.cmd) in

      let this_pid =
        match Core_unix.fork () with
        | `In_the_child ->
            Core_unix.close read_stdout;
            Core_unix.close read_stderr;
            if phys_equal write_stdout proc.stdout then ()
            else Core_unix.close write_stdout;
            if phys_equal write_stderr proc.stderr then ()
            else Core_unix.close write_stderr;

            Core_unix.dup2 ~src:proc.stdin ~dst:Core_unix.stdin ();
            Core_unix.dup2 ~src:proc.stdout ~dst:Core_unix.stdout ();
            Core_unix.dup2 ~src:proc.stderr ~dst:Core_unix.stderr ();
            let env = List.of_array env in
            (* TODO delete *)
            let _ =
              Core_unix.exec ~env:(`Replace_raw env) ~use_path:true ~prog
                ~argv:proc.cmd ()
            in
            failwith "Unreachable"
        | `In_the_parent pid ->
            List.iter proc.pipes_to_collect ~f:(fun pipe ->
                Core_unix.close pipe);
            pid
      in
      this_pid :: prev_pids
    in

    let pids = get_pids proc in

    Core_unix.close write_stdout;
    Core_unix.close write_stderr;

    (* See BUFSIZ in stdio.h *)
    let bufsiz = 8192 in

    let stdout_buf = Buffer.create 16 in
    let stderr_buf = Buffer.create 16 in
    let rec select input_fds =
      (* Should this even have a timeout? *)
      let ready_fds =
        (Core_unix.select
           ~timeout:(`After (Time_ns.Span.create ~sec:10 ()))
           ~except:[] ~write:[]
           ~read:[ read_stdout; read_stderr ]
           ())
          .read
      in
      let buf = Bytes.create bufsiz in
      let stdout_chan = Core_unix.out_channel_of_descr original_stdout in
      let stderr_chan = Core_unix.out_channel_of_descr original_stderr in
      let input_fds =
        List.fold_left ~init:input_fds ready_fds ~f:(fun input_fds fd ->
            let bytes_read = Core_unix.read ~pos:0 ~len:bufsiz ~buf fd in
            if bytes_read = 0 then
              let updated_fds =
                List.filter input_fds ~f:(fun other_fd ->
                    not @@ Core_unix.File_descr.equal other_fd fd)
              in
              updated_fds
            else
              let chan, string_buf =
                let ( === ) = Core_unix.File_descr.equal in
                if fd === read_stdout then (stdout_chan, stdout_buf)
                else if fd === read_stderr then (stderr_chan, stderr_buf)
                else failwith "Unreachable"
              in
              Out_channel.output chan ~buf ~pos:0 ~len:bytes_read;
              Out_channel.flush chan;
              Buffer.add_subbytes string_buf buf ~pos:0 ~len:bytes_read;
              input_fds)
      in
      if List.is_empty input_fds then
        (Buffer.contents stdout_buf, Buffer.contents stderr_buf)
      else select input_fds
    in

    (* TODO check $Process.tee *)
    let stdout, stderr = select [ read_stdout; read_stderr ] in

    (* First is the last in the queue *)
    let last_pid = List.hd_exn pids in
    (* TODO support non-zero exit codes *)
    match Core_unix.waitpid last_pid with
    | Error _ -> Error "Your subprocess failed with a mysterious(?) error"
    | Ok () ->
        (* TODO check all the other pids too *)
        Ok (Runtime.ProcessResult { code = 0; stdout; stderr })
end

module type TestSig = sig
  include Sig

  type fs_entity = File of string | Directory

  val stdout_buffer : string list ref
  val file_system : (string, fs_entity) Hashtbl.t
  val proc_expectations : Mock_process.spec option ref
end

module Make_test () : TestSig = struct
  type fs_entity = File of string | Directory

  let stdout_buffer : string list ref = ref []
  let file_system = Hashtbl.create (module String)
  let proc_expectations : Mock_process.spec option ref = ref None
  let chdir _ = ()

  let mkdir path =
    match Hashtbl.add file_system ~key:path ~data:Directory with
    | `Ok -> ()
    | `Duplicate ->
        Printf.sprintf "EEXIST: the directory %s already exists" path
        |> failwith

  let directory_exists path =
    let entity_opt = Hashtbl.find file_system path in
    match entity_opt with
    | None -> false
    | Some entity -> ( match entity with File _ -> false | Directory -> true)

  let file_write_all path ~data =
    (* TODO allow over-writing *)
    Hashtbl.add_exn file_system ~key:path ~data:(File data)

  let file_read_all path =
    (* TODO: resolve relative paths *)
    let file =
      Hashtbl.find file_system path
      |> Option.value_exn
           ~message:(Printf.sprintf "Error no entity \"%s\"" path)
    in
    match file with File data -> data | _ -> failwith "TODO"

  let print_s s = stdout_buffer := s :: !stdout_buffer

  let rec proc_exec (proc : Runtime.process) env =
    (* Start from the end of the list *)
    (match proc.previous with
    | Some prev ->
        let _ = proc_exec prev env in
        ()
    | None -> ());
    match !proc_expectations |> Option.value_exn with
    | [] ->
        Printf.sprintf "Unexpected proc: %s"
        @@ Runtime.to_s (Runtime.Process proc)
        |> failwith
    | hd :: tl -> (
        proc_expectations := Some tl;
        let n = List.compare String.compare hd.cmd proc.cmd in
        match n with
        | 0 ->
            let code = ref 0 in
            let stdout_buf = Buffer.create 32 in
            let stderr_buf = Buffer.create 32 in
            List.iter hd.instructions ~f:(function
              | Exit c -> code := c
              | Stdout s -> Buffer.add_string stdout_buf s
              | Stderr s -> Buffer.add_string stderr_buf s
              | Stdin _ -> failwith "TODO");
            Ok
              (Runtime.ProcessResult
                 {
                   code = !code;
                   stdout = Buffer.contents stdout_buf;
                   stderr = Buffer.contents stderr_buf;
                 })
        | _ -> Error "TODO")
  (* TODO interpret the instructions *)
end

let make_test spec =
  let module M = Make_test () in
  M.proc_expectations := Some spec;
  (module M : TestSig)
