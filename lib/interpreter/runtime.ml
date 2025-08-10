open Core

type t =
  | String of string
  | Bool of bool
  | Num of float
  | List of t Array.t
  | HashMap of (t, t) Stdlib.Hashtbl.t
  | Null
  | Func of function_t

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

let rec to_s = function
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
  | Func _ -> "func(TODO)"

let num_of_val = function Num f -> Some f | _ -> None

let int_of_val v =
  num_of_val v
  |> Option.bind ~f:(fun f ->
         if Float.is_integer f then Some (Int.of_float f) else None)

let bool_of_val = function Bool b' -> Some b' | _ -> None

(*
let invoke_method lookup receiver method_name args =
  let not_implemented receiver =
    Error
      (Printf.sprintf "The type %s does not implement the method %s" receiver
         method_name)
  in
  let check_arity desired_count actual_list name =
    let actual_count = List.length actual_list in
    if not (desired_count = actual_count) then
      Some
        (Error
           (Printf.sprintf
              "The function %s expected %d arguments but received %d" name
              desired_count actual_count))
    else None
  in

  match receiver with
  | Null -> Error "NPE!"
  | String _ -> not_implemented "String"
  | Num f -> (
    let klass = Hashtbl.find lookup "Number" |> Option.value_exn in
    List.iter klass.methods ~f:(function
      | User _ -> failwith "Unreachable"
      | Native {parameters; cb; identifiers} -> )
in
      (* Does it matter this is O(n)? *)
      match method_name with
      | "+" -> (
          match check_arity 1 args "Number.+" with
          | Some e -> e
          | None -> (
              let arg = List.hd_exn args in
              match num_of_val arg with
              | Some arg_f -> Ok (Num (f +. arg_f))
              | None ->
                  Error
                    (Printf.sprintf
                       "The \"+\" method expects a Number argument, but it \
                        instead received \"%s\""
                       (to_s arg))))
      | "-" -> (
          match check_arity 1 args "Number.-" with
          | Some e -> e
          | None -> (
              let arg = List.hd_exn args in
              match num_of_val arg with
              | Some arg_f -> Ok (Num (f -. arg_f))
              | None ->
                  Error
                    (Printf.sprintf
                       "The \"-\" method expects a Number argument, but it \
                        instead received \"%s\""
                       (to_s arg))))
      | "*" -> (
          match check_arity 1 args "Number.*" with
          | Some e -> e
          | None -> (
              let arg = List.hd_exn args in
              match num_of_val arg with
              | Some arg_f -> Ok (Num (f *. arg_f))
              | None ->
                  Error
                    (Printf.sprintf
                       "The \"*\" method expects a Number argument, but it \
                        instead match received \"%s\""
                       (to_s arg))))
      | "/" -> (
          match check_arity 1 args "Number./" with
          | Some e -> e
          | None -> (
              let arg = List.hd_exn args in
              match num_of_val arg with
              | Some arg_f -> Ok (Num (f /. arg_f))
              | None ->
                  Error
                    (Printf.sprintf
                       "The \"/\" method expects a Number argument, but it \
                        instead received \"%s\""
                       (to_s arg))))
      | "<=" -> (
          match check_arity 1 args "Number.<=" with
          | Some e -> e
          | None -> (
              let arg = List.hd_exn args in
              match num_of_val arg with
              | Some arg_f ->
                  let b = Bool (Float.( <= ) f arg_f) in
                  Ok b
              | None ->
                  Error
                    (Printf.sprintf
                       "The \"<=\" method expects a Number argument, but it \
                        instead received \"%s\""
                       (to_s arg))))
      | _ -> not_implemented "Number")
  | _ -> Error "TODO"
  *)
