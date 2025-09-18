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

type process_result = { code : int; stdout : string; stderr : string }
type file = { path : string }

type t =
  | String of string
  | Bool of bool
  | Num of float
  | List of t Array.t
  | HashMap of (t, t) Stdlib.Hashtbl.t
  | Null
  | Func of function_t
  | Method of t * function_t  (** This is created by Object de-referencing *)
  | Prototype of prototype
  | Process of process
  | ProcessResult of process_result
  | File of file
  | FileHandle
  | Directory of string

(* TODO add positions for error messages *)
and function_t =
  (* TODO this prob doesn't need params or identifiers *)
  | Native of {
      parameters : string list;
      cb : t list -> (t, string) Result.t;
      identifiers : t Identifiers.t;
    }
  | User of {
      parameters : string list;
      block : Compiler.Optimizer.stmt list;
      identifiers : t Identifiers.t;
    }

type class_t = {
  instance_members : (string, t) Hashtbl.t;
  static_members : (string, t) Hashtbl.t;
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
  | Prototype _ -> "Prototype"
  | Process _ -> "Process"
  | ProcessResult _ -> "ProcessResult"
  | File _ -> "File"
  | FileHandle -> "FileHandle"
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
      Stdlib.Hashtbl.fold
        (fun key data acc ->
          Printf.sprintf "%s%s: %s," acc (to_s key) (to_s data))
        tbl "{"
      ^ "}"
  | Func _ -> "Func(TODO)"
  | Method (receiver, _) -> Printf.sprintf "%s.Method" (to_s receiver)
  | Prototype { name } -> Printf.sprintf "Type(%s)" name
  (* TODO we should list out all in the group *)
  | Process { cmd; _ } ->
      Printf.sprintf "Process(cmd=[%s])" @@ stringify_list cmd
  | ProcessResult { code; stdout; stderr } ->
      let stdout = String.strip stdout in
      let stderr = String.strip stderr in
      Printf.sprintf "ProcessResult(code=%d, stdout=\"%s\", stderr=\"%s\")" code
        stdout stderr
  | File { path } -> Printf.sprintf "File(path=%s)" path
  | FileHandle -> "FileHandle(TODO)"
  | Directory path -> Printf.sprintf "Directory(path=%s)" path

let num_of_val = function Num f -> Some f | _ -> None
let string_of_val = function String s -> Some s | _ -> None

let int_of_val v =
  num_of_val v
  |> Option.bind ~f:(fun f ->
         if Float.is_integer f then Some (Int.of_float f) else None)

let bool_of_val = function Bool b' -> Some b' | _ -> None
let list_of_val = function List l -> Some l | _ -> None
let hashmap_of_val = function HashMap h -> Some h | _ -> None
let process_of_val = function Process p -> Some p | _ -> None
let process_result_of_val = function ProcessResult p -> Some p | _ -> None
let func_of_val = function Func func -> Some func | _ -> None

let method_of_val = function
  | Method (t, func_t) -> Some (t, func_t)
  | _ -> None

let file_of_val = function File f -> Some f | _ -> None
let directory_of_val = function Directory p -> Some p | _ -> None
