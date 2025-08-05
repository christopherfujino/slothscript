open Core

type t = { pos_fname : string; pos_lnum : int; pos_bol : int; pos_cnum : int }
[@@deriving sexp]

let t_of_lexing_position (p : Lexing.position) : t =
  {
    pos_fname = p.pos_fname;
    pos_lnum = p.pos_lnum;
    pos_bol = p.pos_bol;
    pos_cnum = p.pos_cnum;
  }

let string_of_t _ = Printf.sprintf "[TODO] implement Sloth_common.Position.string_of_t"
