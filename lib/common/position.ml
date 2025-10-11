open Core

let rec indent_str b i s =
  if i <= 0 then Buffer.add_string b s
  else (
    Buffer.add_string b " ";
    (indent_str [@tailcall]) b (i - 1) s)

(* t' src *)
let summarize (t' : Lexing.position) ?(context = 3) ?(show_cursor = true)
    ?(margin_width = 1) src =
  let line_num_col = Printf.sprintf "%d | " t'.pos_lnum in
  let line_num_len = String.length line_num_col in
  let buffer = Buffer.create ((80 * 2) + 2) in
  let lines = String.split_lines src in
  let render_line_with_context last_line_num =
    let rec compute_line_nums prev count cur_line_num =
      if count = 0 then
        let previous_line_trimmed =
          (* since this is an index, we subtract 1 from last_line_num, which
             is cur_line_num *)
          List.nth_exn lines cur_line_num |> String.strip
        in
        if String.length previous_line_trimmed = 0 then
          (* Render an extra line since the last line has no code *)
          (compute_line_nums [@tailcall]) prev 1 cur_line_num
        else prev
      else if cur_line_num = 0 then prev
      else
        (compute_line_nums [@tailcall]) (cur_line_num :: prev) (count - 1)
          (cur_line_num - 1)
    in
    let line_nums = compute_line_nums [] context last_line_num in

    let line_num_strings = List.map line_nums ~f:Int.to_string in
    let width =
      List.fold line_num_strings ~init:0 ~f:(fun len str ->
          let cur_len = String.length str in
          if cur_len > len then cur_len else len)
    in
    List.iter2 line_nums line_num_strings ~f:(fun num num_str ->
        let formatted_num_str =
          String.pad_left ~char:' ' num_str ~len:(width + margin_width)
        in
        Buffer.add_string buffer
        @@ Printf.sprintf "%s | %s\n" formatted_num_str
        @@ List.nth_exn lines (num - 1))
  in
  match render_line_with_context t'.pos_lnum with
  | Unequal_lengths -> Common.internal_failure __LOC__
  | Ok () ->
      (if show_cursor then
         let line_col =
           t'.pos_cnum - t'.pos_bol + line_num_len + margin_width
         in
         indent_str buffer line_col "^");
      Buffer.contents buffer

let string_of_t (t' : Lexing.position) =
  Printf.sprintf "%d:%d" t'.pos_lnum (t'.pos_cnum - t'.pos_bol)
