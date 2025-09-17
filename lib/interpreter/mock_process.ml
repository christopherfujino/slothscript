open Core

type instruction =
  | Stdout of string
  | Stderr of string
  | Stdin of string
  | Exit of int
[@@deriving sexp]

type proc = { cmd : string list; instructions : instruction list }
[@@deriving sexp]

type spec = proc list [@@deriving sexp]

let spec_of_string s = Sexp.of_string s |> spec_of_sexp
let empty_spec = []
