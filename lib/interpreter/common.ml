open Core

type quote_state = None | Single | Double

let shell_like_escape input =
  let buffer_size = 40 in
  let state = ref None in
  let buf = Buffer.create buffer_size in
  let f prev_lines cur_char =
    match cur_char with
    | ' ' -> (
        match !state with
        | None ->
            let bufcontents = Buffer.contents buf in
            Buffer.clear buf;
            bufcontents :: prev_lines
        | _ ->
            Buffer.add_char buf ' ';
            prev_lines)
    | '"' -> (
        match !state with
        | Double ->
            state := None;
            let bufcontents = Buffer.contents buf in
            Buffer.clear buf;
            bufcontents :: prev_lines
        | Single ->
            Buffer.add_char buf cur_char;
            prev_lines
        | None ->
            state := Double;
            prev_lines)
    | '\'' -> (
        match !state with
        | Single ->
            state := None;
            let bufcontents = Buffer.contents buf in
            Buffer.clear buf;
            bufcontents :: prev_lines
        | None ->
            state := Single;
            prev_lines
        | Double ->
            Buffer.add_char buf cur_char;
            prev_lines)
    | _ ->
        Buffer.add_char buf cur_char;
        prev_lines
  in
  let output = String.fold input ~init:[] ~f in
  let output =
    if Buffer.length buf > 0 then Buffer.contents buf :: output else output
  in
  List.rev output
