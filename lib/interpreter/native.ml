open Core
open Sloth_common.Common

type processMode =
  | BlockInherit (* proc! -> null *)
  | ForkInherit (* ? -> ProcessHandle *)
  | ForkBuffer (* proc& -> ProcessHandle *)
  | BlockBuffer (* proc&! -> ProcessResult *)

module type Sig = sig
  (* TODO we should never be returning Runtime.t, but more specific types *)

  val file_read_all : string -> string
  val fd_read_all : Core_unix.File_descr.t -> (Runtime.t, string) Result.t
  val fd_write_all : Core_unix.File_descr.t -> string -> (unit, string) Result.t

  (* TODO does this type of `data` need to be more generic to support binary? *)
  val write : Core_unix.File_descr.t -> data:string -> (unit, string) Result.t
  val read : Core_unix.File_descr.t -> (Runtime.t, string) Result.t
  val wait : Runtime.process_handle -> (Runtime.t, string) Result.t

  val pipe : unit -> Core_unix.File_descr.t * Core_unix.File_descr.t
  (** unit -> read * write *)

  val open_file :
    mode:Core_unix.open_flag list ->
    string ->
    (Core_unix.File_descr.t, string) Result.t

  val close : Core_unix.File_descr.t -> (unit, string) Result.t

  val proc_exec :
    mode:processMode ->
    Runtime.process ->
    string array ->
    (Runtime.t, string) Result.t

  val chdir : string -> (unit, string) Result.t
  val directory_exists : string -> bool
  val file_exists : string -> bool
  val mkdir : string -> unit
end

module Prod : Sig = struct
  let pipe = Core_unix.pipe ~close_on_exec:true
  let file_read_all = In_channel.read_all

  let close fd =
    (* TODO catch? *)
    Ok (Core_unix.close fd)

  let fd_write_all fd contents =
    let len = String.length contents in
    let buf = Bytes.of_string contents in
    (* TODO: Should we hunk this into smaller writes for large inputs? *)
    let n = Core_unix.single_write ~len ~buf ~pos:0 fd in
    if not (n = len) then
      Error (Printf.sprintf "Tried to write %d bytes but only wrote %d" len n)
    else close fd

  let fd_read_all fd =
    let string_buf = Buffer.create bufsiz in
    let buf = Bytes.create bufsiz in
    let rec loop () =
      let n = Core_unix.read ~len:bufsiz fd ~buf in
      if n = 0 then Ok (Runtime.String (Buffer.contents string_buf))
      else (
        Buffer.add_subbytes string_buf buf ~pos:0 ~len:n;
        (loop [@tailcall]) ())
    in
    let result = loop () in
    (* TODO close fd *)
    result

  let read fd =
    let string_buf = Buffer.create bufsiz in
    let buf = Bytes.create bufsiz in
    let n = Core_unix.read ~len:bufsiz fd ~buf in
    Buffer.add_subbytes string_buf buf ~pos:0 ~len:n;
    Ok (Runtime.String (Buffer.contents string_buf))

  let open_file ~mode path =
    try Ok (Core_unix.openfile ~mode path) with
    | Core_unix.Unix_error (err, _, _) ->
        let err_msg = Core_unix.Error.message err in
        Error (Printf.sprintf "`open_file(%s)` failed with \"%s\"" path err_msg)
    | exn -> Exn.to_string exn |> failwith

  let write fd ~data =
    let buf = Bytes.of_string data in
    let len = Bytes.length buf in
    let actual_len = Core_unix.write fd ~buf ~len in
    if not (actual_len = len) then
      Error
        (Printf.sprintf "Tried to write %d bytes, but actually wrote %d" len
           actual_len)
    else Ok ()

  let chdir path =
    try Ok (Core_unix.chdir path)
    with Core_unix.Unix_error (err, _, _) ->
      let err_msg = Core_unix.Error.message err in
      Error (Printf.sprintf "`chdir(%s)` failed with \"%s\"" path err_msg)

  let mkdir = Core_unix.mkdir ~perm:0o775

  let file_exists path =
    match Sys_unix.file_exists ~follow_symlinks:true path with
    | `No -> false
    | `Yes -> true
    | `Unknown -> Printf.sprintf "TODO %s %s" path __LOC__ |> failwith

  let directory_exists path =
    match Sys_unix.is_directory ~follow_symlinks:true path with
    | `Yes -> true
    | `No -> false
    | `Unknown -> Printf.sprintf "TODO %s %s" path __LOC__ |> failwith

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
      | BlockInherit | ForkInherit -> (None, None, None, None)
      | BlockBuffer | ForkBuffer ->
          (* TODO: this should not be a pipe, but a growable buffer to avoid deadlock! *)
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
            | BlockInherit | ForkInherit -> ()
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
            let _ =
              try
                Core_unix.exec ~env:(`Replace_raw env) ~use_path:true ~prog
                  ~argv:proc.cmd ()
              with Core_unix.Unix_error (err, syscall, _) ->
                let _, buf =
                  List.fold proc.cmd
                    ~init:(true, Buffer.create 100)
                    ~f:(fun (is_first, buf) cur ->
                      if not is_first then Buffer.add_string buf " ";
                      Buffer.add_string buf cur;
                      (false, buf))
                in

                Printf.eprintf "Failed to `%s` process `%s`: \"%s\"\n\n%!"
                  syscall (Buffer.contents buf)
                  (Core_unix.Error.message err);
                exit 1
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
    | BlockInherit | ForkInherit -> ()
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
    | ForkInherit ->
        Ok (Runtime.ProcessHandle (Runtime.ProcessInherited last_pid))
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

  type fs_entity = File of string ref * Core_unix.File_descr.t | Directory

  val get_stdout : unit -> string
  val path_to_entity : (string, fs_entity) Hashtbl.t
  val proc_expectations : Mock_process.spec option ref
end

module Make_test () : TestSig = struct
  type fs_entity = File of string ref * Core_unix.File_descr.t | Directory

  let running_pids : (int * (unit -> (Runtime.t, 'a) Result.t)) list ref =
    ref []

  module Fds : sig
    val get : int -> (fs_entity, string) Result.t
    val get_file : int -> (string ref * Core_unix.File_descr.t, string) Result.t
    val remove : int -> (unit, string) Result.t
    val set : int -> fs_entity -> unit
  end = struct
    let open_fds : (int * fs_entity) list ref =
      ref
        [
          (0, File (ref "", Core_unix.File_descr.of_int 0)) (* STDIN *);
          (1, File (ref "", Core_unix.File_descr.of_int 1)) (* STDOUT *);
          (2, File (ref "", Core_unix.File_descr.of_int 2)) (* STDERR *);
        ]

    let set fd entity =
      open_fds := List.Assoc.add !open_fds ~equal:( = ) fd entity

    let get i =
      match List.Assoc.find !open_fds ~equal:( = ) i with
      | None -> Error (Printf.sprintf "File descriptor %d not found" i)
      | Some entity -> Ok entity

    let get_file i =
      let open Result.Monad_infix in
      get i >>= function
      | File (str_ref, fd) -> Ok (str_ref, fd)
      | Directory -> internal_failure __LOC__

    let remove i =
      let open Result.Monad_infix in
      get i >>= fun _ ->
      open_fds := List.Assoc.remove ~equal:( = ) !open_fds i;
      Ok ()
  end

  module OpenPipes = struct
    (** [(read, write) :: ...] *)
    let open_pipes : (int * int) list ref = ref []

    let add ~read ~write = open_pipes := (read, write) :: !open_pipes

    let get_read_from_write write_int =
      let open Option.Monad_infix in
      List.find !open_pipes ~f:(fun (_, write) -> write = write_int)
      >>= fun (read, _) -> Some read
  end

  let get_stdout () =
    match Fds.get_file 1 with
    | Ok (contents_ref, _) -> !contents_ref
    | Error msg -> failwith msg

  let path_to_entity : (string, fs_entity) Hashtbl.t =
    Hashtbl.create (module String)

  (** Use get_next_fd *)
  let next_fd = ref 3

  let get_next_fd () =
    let this_fd = !next_fd in
    next_fd := this_fd + 1;
    this_fd

  (** Use get_next_pid *)
  let next_pid = ref 42

  (* Public *)
  let pipe () =
    let read_int = get_next_fd () in
    let write_int = get_next_fd () in
    OpenPipes.add ~read:read_int ~write:write_int;
    let read = Core_unix.File_descr.of_int read_int in
    let write = Core_unix.File_descr.of_int write_int in
    Fds.set read_int (File (ref "", read));
    Fds.set write_int (File (ref "", write));
    (read, write)

  let close fd =
    let fd_int = Core_unix.File_descr.to_int fd in
    match Fds.remove fd_int with
    | Ok _ as ok -> ok
    | Error msg -> Error (msg ^ " (did you close this FD twice?)")

  let open_file ~mode path =
    let fd = get_next_fd () in
    let open Result.Monad_infix in
    (* TODO exhaustively h andle all flags in mode *)
    if List.exists mode ~f:(function Core_unix.O_RDONLY -> true | _ -> false)
    then
      match Hashtbl.find path_to_entity path with
      | None ->
          Error
            (Printf.sprintf "no file named %s found in the file system" path)
      | Some entity -> (
          match entity with
          | Directory ->
              Error
                (Printf.sprintf "You tried to open the directory %s as a file"
                   path)
          | File (contents, _) ->
              let fd_t = Core_unix.File_descr.of_int fd in
              let entity = File (contents, fd_t) in
              Fds.set fd entity;
              Ok fd_t)
    else
      let entity = File (ref "", Core_unix.File_descr.of_int fd) in
      Fds.set fd entity;
      (match Hashtbl.add path_to_entity ~key:path ~data:entity with
        | `Ok -> Ok ()
        | `Duplicate ->
            Printf.sprintf "duplicate path %s in test memory file system" path
            |> failwith)
      >>= fun () ->
      let fd = Core_unix.File_descr.of_int fd in
      if
        List.exists mode ~f:(function Core_unix.O_WRONLY -> true | _ -> false)
      then
        (* TODO check for O_CREAT *)
        Ok fd
      else failwith "TODO"

  let get_next_pid () =
    let this_pid = !next_pid in
    next_pid := this_pid + 1;
    this_pid

  let proc_expectations : Mock_process.spec option ref = ref None
  let chdir _ = Ok ()
  (* This function only exists to cause OS side-effects, no-op in tests *)

  let mkdir path =
    match Hashtbl.add path_to_entity ~key:path ~data:Directory with
    | `Ok -> ()
    | `Duplicate ->
        Printf.sprintf "EEXIST: the directory %s already exists" path
        |> failwith

  let file_exists path =
    let entity_opt = Hashtbl.find path_to_entity path in
    match entity_opt with
    | None -> false
    | Some entity -> ( match entity with File _ -> true | Directory -> false)

  let directory_exists path =
    let entity_opt = Hashtbl.find path_to_entity path in
    match entity_opt with
    | None -> false
    | Some entity -> ( match entity with File _ -> false | Directory -> true)

  let write fd ~data =
    let fd = Core_unix.File_descr.to_int fd in
    (* Check if this is write end of a pipe... *)
    let fd =
      match OpenPipes.get_read_from_write fd with
      | None -> fd
      | Some write_pipe -> write_pipe
    in
    let open Result.Monad_infix in
    Fds.get fd >>= function
    | File (contents, _) ->
        (* TODO check permissions FD was opened with *)
        Ok (contents := !contents ^ data)
    | Directory -> internal_failure __LOC__

  (* TODO delete this, just use write *)
  let fd_write_all fd data =
    let original_fd_int = Core_unix.File_descr.to_int fd in
    (* Check if this is read end of a pipe... *)
    let fd_int =
      match OpenPipes.get_read_from_write original_fd_int with
      | None -> original_fd_int
      | Some read_end -> read_end
    in
    let open Result.Monad_infix in
    Fds.get fd_int >>= function
    | File (contents, _) ->
        Fds.remove original_fd_int >>= fun () -> Ok (contents := data)
    | Directory -> internal_failure __LOC__

  let fd_read_all fd =
    let fd_int = Core_unix.File_descr.to_int fd in
    let open Result.Monad_infix in
    Fds.get fd_int >>= function
    | File (contents, _) ->
        Fds.remove fd_int >>= fun () -> Ok (Runtime.String !contents)
    | Directory -> internal_failure __LOC__

  let read fd =
    let fd_int = Core_unix.File_descr.to_int fd in

    let open Result.Monad_infix in
    Fds.get fd_int >>= function
    | File (contents, _) ->
        let str = !contents in
        contents := "";
        (* TODO split by newlines *)
        Ok (Runtime.String str)
    | Directory -> internal_failure __LOC__

  let file_read_all path =
    (* TODO: resolve relative paths *)
    let file =
      Hashtbl.find path_to_entity path
      |> option_value ~message:(Printf.sprintf "Error no entity \"%s\"" path)
    in
    match file with File (data, _) -> !data | _ -> failwith "TODO"

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
             "Tried to wait the PID %d but it is not running; did you `wait()` \
              it twice?\n\n\
              Currently Running PIDs:[%s]"
             pid
             (List.fold ~init:"" !running_pids ~f:(fun prev (pid, _) ->
                  Printf.sprintf "%s %d" prev pid)))
    | Some res -> res

  let proc_exec ~mode proc _ =
    let rec rec_proc_exec (proc : Runtime.process) =
      (* Start from the end of the list *)
      (match proc.previous with
      | Some prev ->
          let _ = rec_proc_exec prev in
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
              let exec_proc_spec stdout stderr =
                let code = ref 0 in
                List.iter hd.instructions ~f:(function
                  | Exit c -> code := c
                  | Stdout s -> Result.ok_or_failwith (write stdout ~data:s)
                  | Stderr s -> Result.ok_or_failwith (write stderr ~data:s)
                  | Stdin _ -> failwith "TODO");
                !code
              in
              match mode with
              | BlockInherit ->
                  let _ = exec_proc_spec proc.stdout proc.stderr in
                  Ok Runtime.Null
              | BlockBuffer ->
                  let read_stdout, write_stdout = pipe () in
                  let read_stderr, write_stderr = pipe () in
                  let code = exec_proc_spec write_stdout write_stderr in
                  let open Result.Monad_infix in
                  fd_read_all read_stdout >>= fun stdout_t ->
                  let stdout =
                    Runtime.string_of_val stdout_t |> Option.value_exn
                  in
                  fd_read_all read_stderr >>= fun stderr_t ->
                  let stderr =
                    Runtime.string_of_val stderr_t |> Option.value_exn
                  in
                  let result = Runtime.ProcessResult { code; stdout; stderr } in
                  Ok result
              | ForkInherit ->
                  let this_pid = get_next_pid () in
                  let callback () =
                    let _ = exec_proc_spec proc.stdout proc.stderr in
                    Ok Runtime.Null
                  in
                  running_pids := (this_pid, callback) :: !running_pids;
                  Ok
                    Runtime.(
                      ProcessHandle (ProcessInherited (Pid.of_int this_pid)))
              | ForkBuffer ->
                  let this_pid = get_next_pid () in
                  let read_stdout, write_stdout = pipe () in
                  let read_stderr, write_stderr = pipe () in
                  let callback () =
                    let code = exec_proc_spec write_stdout write_stderr in
                    let open Result.Monad_infix in
                    fd_read_all read_stdout >>= fun stdout_t ->
                    let stdout =
                      Runtime.string_of_val stdout_t |> Option.value_exn
                    in
                    fd_read_all read_stderr >>= fun stderr_t ->
                    let stderr =
                      Runtime.string_of_val stderr_t |> Option.value_exn
                    in
                    let result =
                      Runtime.ProcessResult { code; stdout; stderr }
                    in
                    Ok result
                  in

                  running_pids := (this_pid, callback) :: !running_pids;
                  Ok
                    Runtime.(
                      ProcessHandle
                        (ProcessBuffered
                           {
                             pid = Pid.of_int this_pid;
                             stdout = read_stdout;
                             stderr = read_stderr;
                           })))
          | _ ->
              let msg =
                Printf.sprintf "Tried to execute sub-process %s but expected %s"
                  (List.to_string ~f:Fun.id proc.cmd)
                  (List.to_string ~f:Fun.id hd.cmd)
              in
              Error msg)
    in
    rec_proc_exec proc
end

let make_test spec =
  let module M = Make_test () in
  M.proc_expectations := Some spec;
  (module M : TestSig)
