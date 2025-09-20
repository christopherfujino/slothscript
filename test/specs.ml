open Core
open Common

let make_spec ~program ~ast ?(stdout_expect = "") ?failure name =
  let stdout_expect = String.strip stdout_expect in
  { name; program; ast; stdout_expect; failure; proc_spec = "()" }

let green () =
  let specs = find_child_specs "./green_specs" in
  List.map specs ~f:Spec_parser.deserialize

let red () =
  let specs = find_child_specs "./red_specs" in
  List.map specs ~f:Spec_parser.deserialize
