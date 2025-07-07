open Core

exception Error of string

let rec indent_str b i s =
  if i <= 0 then Buffer.add_string b s
  else (
    Buffer.add_string b "  ";
    (indent_str [@tailcall]) b (i - 1) s)

let rec sexp_formatter_inner parent_indent indent buffer (s : Sexp.t) =
  let print = indent_str buffer indent in
  match s with
  | Atom a -> print a
  | List l -> (
      match List.length l with
      | 0 -> print "()"
      | 1 ->
          indent_str buffer indent "(";
          let el = List.hd_exn l in
          sexp_formatter_inner parent_indent 0 buffer el;
          Buffer.add_string buffer ")"
      | _ ->
          print "(";
          let rec f l is_first =
            match l with
            | [] -> ()
            | hd :: tail ->
                if is_first then sexp_formatter_inner parent_indent 0 buffer hd
                else (
                  Buffer.add_string buffer "\n";
                  let next_indent = parent_indent + 1 in
                  sexp_formatter_inner next_indent next_indent buffer hd);
                (f [@tailcall]) tail false
          in
          f l true;
          Buffer.add_string buffer ")")

let sexp_formatter str =
  let b = Buffer.create 20 in
  let s = try Sexp.of_string str with Failure msg -> raise (Error msg) in
  sexp_formatter_inner 0 0 b s;
  Buffer.contents b
