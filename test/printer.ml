open Core

exception Error of string

let rec indent_str b i s =
  if i <= 0 then Buffer.add_string b s
  else (
    Buffer.add_string b "  ";
    (indent_str [@tailrec]) b (i - 1) s)

let rec sexp_formatter_inner indent buffer (s : Sexp.t) =
  match s with
  | Atom a -> indent_str buffer indent a
  | List l ->
      indent_str buffer indent "(\n";
      let rec f l is_first =
        if not is_first then Buffer.add_string buffer "\n";
        match l with
        | [] -> ()
        | hd :: tail ->
            sexp_formatter_inner (indent + 1) buffer hd;
            (f [@tailrec]) tail false
      in
      f l true;
      indent_str buffer indent ")"

let sexp_formatter str =
  let b = Buffer.create 20 in
  let s = try Sexp.of_string str with Failure msg -> raise (Error msg) in
  sexp_formatter_inner 0 b s;
  Buffer.contents b
