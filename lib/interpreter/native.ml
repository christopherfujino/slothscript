open Core
open Sloth_common.Common

type processMode =
  | BlockInherit (* proc! -> null *)
  | ForkBuffer (* proc& -> ProcessHandle *)
  | BlockBuffer (* proc&! -> ProcessResult *)

module type Sig = sig
  val print_s : string -> unit
  val file_read_all : string -> string

  (* TODO does this type of `data` need to be more generic to support binary? *)
  val write : Core_unix.File_descr.t -> data:string -> (unit, string) Result.t
  val wait : Runtime.process_handle -> (Runtime.t, string) Result.t

  val proc_exec :
    mode:processMode ->
    Runtime.process ->
    string array ->
    (Runtime.t, string) Result.t

  val chdir : string -> unit
  val directory_exists : string -> bool
  val mkdir : string -> unit
end

module Prod : Sig = struct
  let print_s = Printf.printf "%s%!"
  let file_read_all = In_channel.read_all

  let write fd ~data =
    let buf = Bytes.of_string data in
    let len = Bytes.length buf in
    let actual_len = Core_unix.write fd ~buf ~len in
    if not (actual_len = len) then
      Error
        (Printf.sprintf "Tried to write %d bytes, but actually wrote %d" len
           actual_len)
    else Ok ()

  let chdir = Core_unix.chdir
  let mkdir = Core_unix.mkdir ~perm:0o775

  let directory_exists path =
    match Sys_unix.is_directory ~follow_symlinks:true path with
    | `Yes -> true
    | `No -> false
    | `Unknown -> false (* TODO? *)

  let rec select input_fds read_stdout read_stderr stdout_buf stderr_buf =
    (* Should this even have a timeout? *)
    let ready_fds =
      (Core_unix.select ~timeout:`Never ~except:[] ~write:[] ~read:input_fds ())
        .read
    in
    let buf = Bytes.create bufsiz in
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
            let string_buf =
              let ( === ) = Core_unix.File_descr.equal in
              if fd === read_stdout then stdout_buf
              else if fd === read_stderr then stderr_buf
              else failwith "Unreachable"
            in
            Buffer.add_subbytes string_buf buf ~pos:0 ~len:bytes_read;
            input_fds)
    in
    if List.is_empty input_fds then
      (Buffer.contents stdout_buf, Buffer.contents stderr_buf)
    else select input_fds read_stdout read_stderr stdout_buf stderr_buf

  let wait handle =
    let pid =
      Runtime.(
        match handle with
        | ProcessBuffered { pid; _ } -> pid
        | ProcessInherited pid -> pid)
    in
    match Core_unix.waitpid pid with
    | Error e -> (
        match e with
        | `Exit_non_zero code ->
            Error (Printf.sprintf "Your subprocess exited with code %d" code)
        | `Signal s ->
            Error
              (Printf.sprintf "Your subprocess exited on signal %s"
              @@ Signal.to_string s))
    | Ok () -> (
        (* TODO check all the other pids too *)
        match handle with
        | ProcessBuffered { pid = _; stdout; stderr } ->
            let stdout_buf = Buffer.create 512 in
            let stderr_buf = Buffer.create 512 in

            let stdout, stderr =
              select [ stdout; stderr ] stdout stderr stdout_buf stderr_buf
            in
            Ok (Runtime.ProcessResult { code = 0; stdout; stderr })
        | ProcessInherited _ -> Ok Runtime.Null)

  let proc_exec ~mode (proc : Runtime.process) env =
    let read_stdout, write_stdout, read_stderr, write_stderr =
      match mode with
      | BlockInherit -> (None, None, None, None)
      | BlockBuffer | ForkBuffer ->
          let read_stdout, write_stdout =
            Core_unix.pipe ~close_on_exec:true ()
          in
          let read_stderr, write_stderr =
            Core_unix.pipe ~close_on_exec:true ()
          in
          proc.stdout <- write_stdout;
          proc.stderr <- write_stderr;
          ( Some read_stdout,
            Some write_stdout,
            Some read_stderr,
            Some write_stderr )
    in
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
            (match mode with
            | BlockInherit -> ()
            | BlockBuffer | ForkBuffer ->
                let read_stdout = option_value read_stdout ~message:__LOC__ in
                let read_stderr = option_value read_stderr ~message:__LOC__ in
                let write_stdout = option_value write_stdout ~message:__LOC__ in
                let write_stderr = option_value write_stderr ~message:__LOC__ in
                Core_unix.close read_stdout;
                Core_unix.close read_stderr;
                if phys_equal write_stdout proc.stdout then ()
                else Core_unix.close write_stdout;
                if phys_equal write_stderr proc.stderr then ()
                else Core_unix.close write_stderr);

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

    (match mode with
    | BlockInherit -> ()
    | BlockBuffer | ForkBuffer ->
        Core_unix.close @@ option_value write_stdout ~message:__LOC__;
        Core_unix.close @@ option_value write_stderr ~message:__LOC__);

    (* First is the last in the queue *)
    let last_pid = List.hd_exn pids in

    let open Result.Monad_infix in
    match mode with
    | BlockInherit (* ! *) ->
        let handle = Runtime.ProcessInherited last_pid in
        (* This should already return Ok Runtime.Null on success *)
        wait handle
    | ForkBuffer ->
        Ok
          (Runtime.ProcessHandle
             (ProcessBuffered
                {
                  pid = last_pid;
                  stdout = option_value read_stdout ~message:__LOC__;
                  stderr = option_value read_stderr ~message:__LOC__;
                }))
    | BlockBuffer ->
        let handle =
          Runtime.ProcessBuffered
            {
              pid = last_pid;
              stdout = option_value read_stdout ~message:__LOC__;
              stderr = option_value read_stderr ~message:__LOC__;
            }
        in
        wait handle >>= fun t -> Ok t
end

module type TestSig = sig
  include Sig

  type fs_entity = File of string | Directory

  val stdout_buffer : string list ref
  val path_to_fd : (string, int) Hashtbl.t
  val fd_to_entity : (int, fs_entity) Hashtbl.t
  val proc_expectations : Mock_process.spec option ref
end

module Make_test () : TestSig = struct
  type fs_entity = File of string | Directory

  let stdout_buffer : string list ref = ref []

  let running_pids : (int * (unit -> (Runtime.t, 'a) Result.t)) list ref =
    ref []

  (** Use get_next_fd *)
  let next_fd = ref 3

  let get_next_fd () =
    let this_fd = !next_fd in
    next_fd := this_fd + 1;
    this_fd

  (** Use get_next_pid *)
  let next_pid = ref 42

  let get_next_pid () =
    let this_pid = !next_pid in
    next_pid := this_pid + 1;
    this_pid

  let path_to_fd : (string, int) Hashtbl.t = Hashtbl.create (module String)
  let fd_to_entity : (int, fs_entity) Hashtbl.t = Hashtbl.create (module Int)
  let proc_expectations : Mock_process.spec option ref = ref None
  let chdir _ = ()
  (* This function only exists to cause OS side-effects, no-op in tests *)

  let mkdir path =
    let fd = get_next_fd () in
    (match Hashtbl.add path_to_fd ~key:path ~data:fd with
    | `Ok -> ()
    | `Duplicate ->
        Printf.sprintf "EEXIST: the directory %s already exists" path
        |> failwith);
    match Hashtbl.add fd_to_entity ~key:fd ~data:Directory with
    | `Ok -> ()
    | `Duplicate -> internal_failure __LOC__

  let directory_exists path =
    let entity_opt = Hashtbl.find file_system path in
    match entity_opt with
    | None -> false
    | Some entity -> ( match entity with File _ -> false | Directory -> true)

  let write fd ~data =
    let fd = Core_unix.File_descr.to_int fd in
    (* TODO allow over-writing *)
    Hashtbl.add_exn fd_to_entity ~key:fd ~data:(File data);
    Ok ()

  let file_read_all path =
    (* TODO: resolve relative paths *)
    let file =
      Hashtbl.find file_system path
      |> option_value ~message:(Printf.sprintf "Error no entity \"%s\"" path)
    in
    match file with File data -> data | _ -> failwith "TODO"

  let print_s s = stdout_buffer := s :: !stdout_buffer

  let wait handle =
    let pid =
      Runtime.(
        match handle with
        | ProcessBuffered { pid; _ } -> pid
        | ProcessInherited pid -> pid)
      |> Pid.to_int
    in
    let still_running_pids, res_opt =
      List.fold !running_pids ~init:([], None)
        ~f:(fun (prev_pids, res_opt) (cur_pid, cb) ->
          if Int.(cur_pid = pid) then
            let res = cb () in
            (prev_pids, Some res)
          else ((cur_pid, cb) :: prev_pids, res_opt))
    in
    running_pids := still_running_pids;
    match res_opt with
    | None ->
        (* TODO should this just be a no-op? *)
        Error
          (Printf.sprintf
             "Tried to wait the PID %d but it is not running; did you \
              `wait()`'d it twice"
             pid)
    | Some res -> res

  let proc_exec ~mode proc _ =
    let rec rec_proc_exec follower_opt (proc : Runtime.process) =
      (* Start from the end of the list *)
      (match proc.previous with
      | Some prev ->
          let _ = rec_proc_exec (Some proc) prev in
          ()
      | None -> ());
      match !proc_expectations |> option_value ~message:__LOC__ with
      | [] ->
          Printf.sprintf "Unexpected proc: %s"
          @@ Runtime.to_s (Runtime.Process proc)
          |> failwith
      | hd :: tl -> (
          proc_expectations := Some tl;
          let n = List.compare String.compare hd.cmd proc.cmd in
          match n with
          | 0 -> (
              let exec_proc_spec () =
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
              in
              match mode with
              | BlockInherit ->
                  let open Result.Monad_infix in
                  exec_proc_spec () >>= fun proc_res ->
                  let proc_res =
                    Runtime.process_result_of_val proc_res
                    |> option_value ~message:__LOC__
                  in
                  (* TODO stderr *)
                  (match follower_opt with
                  | None ->
                      proc_res.stdout |> print_s;
                      print_s "\n"
                  | Some _ -> (* TODO match with stdin *) ());
                  Ok Runtime.Null
              | BlockBuffer -> exec_proc_spec ()
              | ForkBuffer ->
                  let this_pid = get_next_pid () in
                  running_pids := (this_pid, exec_proc_spec) :: !running_pids;
                  (* TODO remove this when we've abstracted over this exact type *)
                  let unsafe_fd = Core_unix.File_descr.of_int 69 in
                  Ok
                    Runtime.(
                      ProcessHandle
                        (ProcessBuffered
                           {
                             pid = Pid.of_int this_pid;
                             stdout = unsafe_fd;
                             stderr = unsafe_fd;
                           })))
          | _ ->
              let msg =
                Printf.sprintf "Tried to execute sub-process %s but expected %s"
                  (List.to_string ~f:Fun.id hd.cmd)
                  (List.to_string ~f:Fun.id proc.cmd)
              in
              Error msg)
    in
    rec_proc_exec None proc
end

let make_test spec =
  let module M = Make_test () in
  M.proc_expectations := Some spec;
  (module M : TestSig)
