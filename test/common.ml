open Core

type test_spec = {
  name : string;
  program : string;
  ast : string;
  stdout_expect : string;
  failure : failure_t option;
}

and failure_t = Parser_error | Optimizer_error | Runtime_error

let string_of_failure = function
  | Parser_error -> "Parser_error"
  | Optimizer_error -> "Optimizer_error"
  | Runtime_error -> "Runtime_error"

let find_child_specs dir_path =
  let dir_fd = Core_unix.opendir dir_path in
  let rec inner_rec acc dir_fd =
    let res_opt = Core_unix.readdir_opt dir_fd in
    match res_opt with
    | None -> acc
    | Some name ->
        if Filename.check_suffix name "sloth" then
          inner_rec (name :: acc) dir_fd
        else inner_rec acc dir_fd
  in
  (* TODO handle whether or not dir_path ends in path separator *)
  List.map (inner_rec [] dir_fd) ~f:(fun base ->
      Printf.sprintf "%s/%s" dir_path base)
