open Core

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

type t =
  (* Primitives *)
  | String of string
  | Bool of bool
  | Num of float
  | Null
  (* Collections *)
  | List of t Array.t
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
  | Error of t
  | Exit of int

and function_t =
  | Native of {
      cb : t Context.t -> t list -> (t, breaking_type) Either.t;
      name : string;
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

let create_error s = Error (String s)

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
      Array.fold ~f:cb ~init:"[" l ^ "]"
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
let list_of_val = function List l -> Some l | _ -> None
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
