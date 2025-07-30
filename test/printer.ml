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
  | Atom a -> (
      let whitespace_opt =
        String.find a ~f:(fun c ->
            match c with
            | ' ' -> true
            | '\t' -> true
            | '\r' -> true
            | '\n' -> true
            | _ -> false)
      in
      match whitespace_opt with
      (* Escape if our atom includes whitespace *)
      | Some _ -> print (Printf.sprintf "\"%s\"" a)
      | None -> if String.length a = 0 then print "\"\"" else print a)
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
  let b = Buffer.create 64 in
  let sexp_tree =
    try Sexp.of_string str with Failure msg -> raise (Error msg)
  in
  sexp_formatter_inner 0 0 b sexp_tree;
  Buffer.contents b
