open Core

let rec indent_str b i s =
  if i <= 0 then Buffer.add_string b s
  else (
    Buffer.add_string b " ";
    (indent_str [@tailcall]) b (i - 1) s)

(* t' src *)
let summarize (t' : Lexing.position) src =
  let line_num_col = Printf.sprintf "%d | " t'.pos_lnum in
  let line_num_len = String.length line_num_col in
  let buffer = Buffer.create ((80 * 2) + 2) in
  Buffer.add_string buffer line_num_col;
  let lines = String.split_lines src in
  let line = List.nth_exn lines (t'.pos_lnum - 1) in
  Buffer.add_string buffer line;
  Buffer.add_char buffer '\n';
  let line_col = t'.pos_cnum - t'.pos_bol + line_num_len in
  indent_str buffer line_col "^";
  Buffer.contents buffer

let string_of_t (t' : Lexing.position) =
  Printf.sprintf "%d:%d" t'.pos_lnum (t'.pos_cnum - t'.pos_bol)
