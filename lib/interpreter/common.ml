open Core

exception Failure of string

let shell_like_escape input =
  let buffer_size = 40 in
  let buf = Buffer.create buffer_size in
  let f prev_lines cur_char =
    match cur_char with
    | ' ' ->
        let bufcontents = Buffer.contents buf in
        Buffer.clear buf;
        bufcontents :: prev_lines
    | _ ->
        Buffer.add_char buf cur_char;
        prev_lines
  in
  let output = String.fold input ~init:[] ~f in
  let output =
    if Buffer.length buf > 0 then Buffer.contents buf :: output else output
  in
  List.rev output
