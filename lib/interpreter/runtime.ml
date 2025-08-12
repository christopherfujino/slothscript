open Core

type prototype = { name : string }
type process = { cmd : string list }

type t =
  | String of string
  | Bool of bool
  | Num of float
  | List of t Array.t
  | HashMap of (t, t) Stdlib.Hashtbl.t
  | Null
  | Func of function_t
  | Prototype of prototype
  | Process of process
  | FileHandle

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

type class_t = { methods : (string, function_t) Hashtbl.t }
type class_lookup = (string, class_t) Hashtbl.t

let to_class_name = function
  | String _ -> "String"
  | Num _ -> "Number"
  | List _ -> "List"
  | Null -> "Null"
  | Bool _ -> "Bool"
  | HashMap _ -> "HashMap"
  | Func _ -> "Function"
  | Prototype _ -> "Prototype"
  | Process _ -> "Process"
  | FileHandle -> "FileHandle"

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
  | Prototype { name } -> Printf.sprintf "Type(%s)" name
  | Process { cmd } -> Printf.sprintf "Process(cmd=[%s])" @@ stringify_list cmd
  | FileHandle -> "FileHandle(TODO)"

let num_of_val = function Num f -> Some f | _ -> None
let string_of_val = function String s -> Some s | _ -> None

let int_of_val v =
  num_of_val v
  |> Option.bind ~f:(fun f ->
         if Float.is_integer f then Some (Int.of_float f) else None)

let bool_of_val = function Bool b' -> Some b' | _ -> None
let list_of_val = function List l -> Some l | _ -> None
let hashmap_of_val = function HashMap h -> Some h | _ -> None

let process_of_val ?cb = function
  | Process p -> p
  | _ -> (
      match cb with
      | Some cb -> cb ()
      | None -> failwith "You should pass a cb to process_of_val")
