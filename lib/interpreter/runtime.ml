open Core
open Sloth_common.Common

type prototype = { name : string }

type process = {
  cmd : string list;
  mutable stdout : Core_unix.File_descr.t;
  mutable stderr : Core_unix.File_descr.t;
  mutable stdin : Core_unix.File_descr.t;
  mutable pipes_to_collect : Core_unix.File_descr.t list;
  previous : process option;
}

type process_handle =
  | ProcessInherited of Pid.t
  | ProcessBuffered of {
      pid : Pid.t;
      stdout : Core_unix.File_descr.t;
      stderr : Core_unix.File_descr.t;
    }  (** A reference to a (potentially) running process. *)

type process_result = { code : int; stdout : string; stderr : string }
type file = { path : string }
type backtrace = (string * Lexing.position) list

let backtrace_to_s ~pos bt src msg type_s =
  let buf = Buffer.create 256 in

  (* Don't print the stacktrace if the stack is empty *)
  if List.length bt > 0 then (
    Buffer.add_string buf "Stacktrace:\n";
    let _ =
      List.fold ~init:0 bt ~f:(fun width (fname, _) ->
          let cur_width = String.length fname in
          if cur_width > width then cur_width else width)
    in
    let rec print_stack = function
      | [] -> ()
      | (fname, pos) :: tl ->
          print_stack tl;
          let down_right_arrow = "\u{21B3}" in
          (* let down_right_arrow = "->" in *)
          Buffer.add_string buf
          @@ Printf.sprintf " %s %s [%s]\n" down_right_arrow fname
          @@ Sloth_common.Position.string_of_t pos;
          Buffer.add_string buf
          @@ Sloth_common.Position.summarize pos ~context:1 ~margin_width:3 src;
          Buffer.add_string buf "\n"
    in
    print_stack bt;
    Buffer.add_char buf '\n');

  Buffer.add_string buf
    (Printf.sprintf "[%s] %s\n\n%s\n%s"
       (Sloth_common.Position.string_of_t pos)
       type_s
       (Sloth_common.Position.summarize pos src)
       msg);
  (if debug_mode then
     let callstack_depth = 50 in
     Buffer.add_string buf
       (* Core.Printexc does not implement .get_callstack *)
       (Stdlib.Printexc.get_callstack callstack_depth
       |> Stdlib.Printexc.raw_backtrace_to_string));
  Buffer.contents buf

type t =
  (* Primitives *)
  | String of string
  | Bool of bool
  | Num of float
  | Null
  (* Collections *)
  | List of t Dynarray.t
  | HashMap of (t, t) Stdlib.Hashtbl.t
  (* Functions *)
  | Func of function_t
  | Method of t * function_t  (** This is created by Object de-referencing *)
  (* Type *)
  | Prototype of prototype
  (* Stdlib Types *)
  | Directory of string
  | File of file
  | FileDescriptor of Core_unix.File_descr.t
  | Pipe of Core_unix.File_descr.t * Core_unix.File_descr.t
  | Process of process
  | ProcessHandle of process_handle
  | ProcessResult of process_result

and breaking_type =
  | Return of t
  | Break of t
  | Continue of t
  | Error of string option * t
  (* This is an option so native implementations can leave it None *)
  | Exit of int

and function_t =
  | Native of {
      cb :
        t Context.t ->
        (args:t list -> function_t -> (t, breaking_type) Either.t) ->
        t list ->
        (t, breaking_type) Either.t;
      name : string; (* TODO: can we delete this? *)
    }
  | User of {
      parameters : string list;
      block : Compiler.Optimizer.stmt list;
      identifiers : t Identifiers.t;
      name : string; (* This is specifically for stacktraces *)
      pos : Lexing.position;
    }

type class_t = {
  instance_getters : (string, t -> (t, string) Result.t) Hashtbl.t;
  instance_setters : (string, t -> t -> (unit, string) Result.t) Hashtbl.t;
  static_getters : (string, t -> (t, string) Result.t) Hashtbl.t;
}

type class_lookup = (string, class_t) Hashtbl.t

let to_class_name = function
  | String _ -> "String"
  | Num _ -> "Number"
  | List _ -> "List"
  | Null -> "Null"
  | Bool _ -> "Bool"
  | HashMap _ -> "HashMap"
  | Func _ -> "Function"
  | Method _ -> "Method"
  | Pipe _ -> "Pipe"
  | Prototype _ -> "Prototype"
  | Process _ -> "Process"
  | ProcessHandle _ -> "ProcessHandle"
  | ProcessResult _ -> "ProcessResult"
  | File _ -> "File"
  | FileDescriptor _ -> "FileDescriptor"
  | Directory _ -> "Directory"

let rec to_s t' =
  let stringify_list l =
    let rec inner is_first acc list =
      match list with
      | [] -> acc
      | hd :: tl ->
          let acc =
            if is_first then acc ^ hd else Printf.sprintf "%s, %s" acc hd
          in
          (inner [@tailcall]) false acc tl
    in
    inner true "" l
  in
  match t' with
  | String s -> s
  | Num f ->
      if Float.is_integer f then Int.of_float f |> Int.to_string
      else Float.to_string f
  | List l ->
      let is_first = ref true in
      let cb acc cur =
        if !is_first then (
          is_first := false;
          acc ^ to_s cur)
        else Printf.sprintf "%s, %s" acc (to_s cur)
      in
      Dynarray.fold_left cb "[" l ^ "]"
  | Null -> "null"
  | Bool b -> if b then "true" else "false"
  | HashMap tbl ->
      let s, _ =
        Stdlib.Hashtbl.fold
          (fun key data (acc, is_first) ->
            let key = to_s key in
            let data = to_s data in
            ( (if is_first then Printf.sprintf "%s%s: %s" acc key data
               else Printf.sprintf "%s, %s: %s" acc key data),
              false ))
          tbl ("{", true)
      in
      s ^ "}"
  | Func _ -> "Func(TODO)"
  | Method (receiver, _) -> Printf.sprintf "%s.Method" (to_s receiver)
  | Pipe (read, write) ->
      Printf.sprintf "Pipe(read=%d, write=%d)"
        (Core_unix.File_descr.to_int read)
        (Core_unix.File_descr.to_int write)
  | Prototype { name } -> Printf.sprintf "Type(%s)" name
  (* TODO we should list out all in the group *)
  | Process { cmd; _ } ->
      Printf.sprintf "Process(cmd=[%s])" @@ stringify_list cmd
  | ProcessHandle proc_handle ->
      let pid =
        match proc_handle with
        | ProcessBuffered { pid; stdout = _; stderr = _ } -> pid
        | ProcessInherited _ -> Sloth_common.Common.internal_failure __LOC__
      in
      Printf.sprintf "ProcessHandle(pid=%s)" @@ Pid.to_string pid
  | ProcessResult { code; stdout; stderr } ->
      let stdout = String.strip stdout in
      let stderr = String.strip stderr in
      Printf.sprintf "ProcessResult(code=%d, stdout=\"%s\", stderr=\"%s\")" code
        stdout stderr
  | File { path } -> Printf.sprintf "File(path=%s)" path
  | FileDescriptor fd ->
      Printf.sprintf "FileDescriptor(fd=%d)" @@ Core_unix.File_descr.to_int fd
  | Directory path -> Printf.sprintf "Directory(path=%s)" path

let num_of_val = function Num f -> Some f | _ -> None
let string_of_val = function String s -> Some s | _ -> None

let int_of_val v =
  num_of_val v
  |> Option.bind ~f:(fun f ->
         if Float.is_integer f then Some (Int.of_float f) else None)

let pipe_of_t = function Pipe (read, write) -> Some (read, write) | _ -> None
let bool_of_val = function Bool b' -> Some b' | _ -> None
let list_of_t = function List l -> Some l | _ -> None
let hashmap_of_val = function HashMap h -> Some h | _ -> None
let process_of_t = function Process p -> Some p | _ -> None
let process_handle_of_t = function ProcessHandle p -> Some p | _ -> None
let process_result_of_val = function ProcessResult p -> Some p | _ -> None
let func_of_val = function Func func -> Some func | _ -> None

let method_of_val = function
  | Method (t, func_t) -> Some (t, func_t)
  | _ -> None

let file_of_t = function File f -> Some f | _ -> None
let file_descriptor_of_t = function FileDescriptor fd -> Some fd | _ -> None
let directory_of_t = function Directory p -> Some p | _ -> None

let val_of_env strings =
  let len = Array.length strings * 2 in
  let map = Stdlib.Hashtbl.create len in
  Array.iter strings ~f:(fun string ->
      let parts = String.split string ~on:'=' in
      match parts with
      | key :: tail ->
          let value = List.fold tail ~init:"" ~f:( ^ ) in
          Stdlib.Hashtbl.add map (String key) (String value)
      | _ ->
          Printf.sprintf "failure to parse the env value \"%s\"" string
          |> failwith);
  HashMap map

let env_of_val = function
  | HashMap map ->
      let len = Stdlib.Hashtbl.length map in
      let arr = Array.create ~len "" in
      let _ =
        Stdlib.Hashtbl.fold
          (fun key v i ->
            let key = string_of_val key |> Option.value_exn in
            (* TODO: user could have done this *)
            let v = string_of_val v |> Option.value_exn in

            let str = Printf.sprintf "%s=%s" key v in
            Array.set arr i str;
            i + 1)
          map 0
      in
      Some arr
  | _ -> None

let rec is_equal is_equality lhs rhs =
  let lh_s = to_class_name lhs in
  let rh_s = to_class_name rhs in
  let same_class = String.equal lh_s rh_s in
  (* if ==, then return false; if !=, then return true *)
  if not same_class then not is_equality
  else
    let ( >>= ) left right = if not left then left else right () in
    match lhs with
    | String lh_s ->
        let rh_s = string_of_val rhs |> option_value ~message:__LOC__ in
        let same_string = String.equal lh_s rh_s in
        Bool.(same_string = is_equality)
    | Num lhs ->
        let rhs = num_of_val rhs |> option_value ~message:__LOC__ in
        let same_float = Float.equal lhs rhs in
        Bool.(same_float = is_equality)
    | Bool lhs ->
        let rhs = bool_of_val rhs |> option_value ~message:__LOC__ in
        let same_bool = Bool.( = ) lhs rhs in
        Bool.( = ) same_bool is_equality
    | List lhs ->
        let rhs = list_of_t rhs |> option_value ~message:__LOC__ in
        let left_len = Dynarray.length lhs in
        let right_len = Dynarray.length rhs in
        let same_list =
          left_len = right_len >>= fun () ->
          let acc = ref true in
          Dynarray.iteri
            (fun i left ->
              acc :=
                !acc >>= fun () ->
                let right = Dynarray.get rhs i in
                is_equal true left right)
            lhs;
          !acc
        in
        Bool.(same_list = is_equality)
    | HashMap lhs ->
        let rhs = hashmap_of_val rhs |> option_value ~message:__LOC__ in
        let left_len = Stdlib.Hashtbl.length lhs in
        let right_len = Stdlib.Hashtbl.length rhs in
        let same_table =
          left_len = right_len >>= fun () ->
          Stdlib.Hashtbl.fold
            (fun key left_value equal_so_far ->
              if not equal_so_far then false
              else
                match Stdlib.Hashtbl.find_opt rhs key with
                | None -> false
                | Some right_value -> is_equal true left_value right_value)
            lhs true
        in
        Bool.(same_table = is_equality)
    | Null -> ( match rhs with Null -> is_equality | _ -> not is_equality)
    | Prototype { name = left_name } -> (
        match rhs with
        | Prototype { name = right_name } ->
            let names_same = String.(left_name = right_name) in
            Bool.(names_same = is_equality)
        | _ -> not is_equality)
    | Process lhs ->
        let rhs = process_of_t rhs |> option_value ~message:__LOC__ in
        let rec inner_proc (lhs : process) (rhs : process) =
          let same_proc =
            (match
               List.fold2 lhs.cmd rhs.cmd ~init:true
                 ~f:(fun all_same left right ->
                   if all_same then String.(left = right) else false)
             with
            | Ok b -> b
            | Unequal_lengths -> false)
            >>= fun () ->
            Core_unix.File_descr.equal lhs.stdout rhs.stdout >>= fun () ->
            Core_unix.File_descr.equal lhs.stderr rhs.stderr >>= fun () ->
            Core_unix.File_descr.equal lhs.stdin rhs.stdin >>= fun () ->
            match
              List.fold2 lhs.pipes_to_collect rhs.pipes_to_collect ~init:true
                ~f:(fun all_same left right ->
                  if all_same then Core_unix.File_descr.equal left right
                  else false)
            with
            | Ok b -> b
            | Unequal_lengths ->
                false >>= fun () ->
                Option.equal
                  (fun proc1 proc2 -> inner_proc proc1 proc2)
                  lhs.previous rhs.previous
          in
          Bool.(same_proc = is_equality)
        in
        inner_proc lhs rhs
    | FileDescriptor lhs ->
        let rhs = file_descriptor_of_t rhs |> option_value ~message:__LOC__ in
        let same_fd = Core_unix.File_descr.equal lhs rhs in
        Bool.(same_fd = is_equality)
    | File { path = lhs } ->
        let rhs = file_of_t rhs |> option_value ~message:__LOC__ in
        let same_file = String.(lhs = rhs.path) in
        Bool.(same_file = is_equality)
    | Directory lhs ->
        let rhs = directory_of_t rhs |> option_value ~message:__LOC__ in
        let same_dir = String.(lhs = rhs) in
        Bool.(same_dir = is_equality)
    | ProcessHandle lhs ->
        let rhs = process_handle_of_t rhs |> option_value ~message:__LOC__ in
        let same_handle =
          match lhs with
          | ProcessInherited left_pid -> (
              match rhs with
              | ProcessInherited right_pid -> Pid.(left_pid = right_pid)
              | _ -> false)
          | ProcessBuffered
              { pid = left_pid; stdout = left_stdout; stderr = left_stderr }
            -> (
              match rhs with
              | ProcessBuffered
                  {
                    pid = right_pid;
                    stdout = right_stdout;
                    stderr = right_stderr;
                  } ->
                  Pid.(left_pid = right_pid) >>= fun () ->
                  Core_unix.File_descr.equal left_stdout right_stdout
                  >>= fun () ->
                  Core_unix.File_descr.equal left_stderr right_stderr
              | _ -> false)
        in
        Bool.(same_handle = is_equality)
    | Pipe (read, write) ->
        let r_read, r_write = pipe_of_t rhs |> option_value ~message:__LOC__ in
        let same_pipe =
          Core_unix.File_descr.(equal read r_read && equal write r_write)
        in
        Bool.(same_pipe = is_equality)
    | ProcessResult _ -> failwith "TODO"
    | Func _ | Method _ ->
        Printf.sprintf "is_equal the type %s is not implemented" lh_s
        |> failwith
