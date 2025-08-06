open Core

type t = { pos_fname : string; pos_lnum : int; pos_bol : int; pos_cnum : int }


let t_of_lexing_position (p : Lexing.position) : t =
  {
    pos_fname = p.pos_fname;
    pos_lnum = p.pos_lnum;
    pos_bol = p.pos_bol;
    pos_cnum = p.pos_cnum;
  }

let sexp_of_t _ = Sexp.Atom "[POS]"

let rec t_of_sexp _ = dummy ()
and dummy () = t_of_lexing_position Lexing.dummy_pos

let string_of_t t' =
  Printf.sprintf "%d:%d" t'.pos_lnum t'.pos_cnum
