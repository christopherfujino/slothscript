open Core
open Common

type state = NotParsing | Parsing of string * string list

let serialize path spec =
  Printf.printf "(1) Serializing %s..." path;
  let buf = Buffer.create 100 in
  let print s =
    Buffer.add_string buf s;
    Buffer.add_char buf '\n'
  in

  print "### Name";
  print spec.name;
  print "\n";

  print "### Program";
  print spec.program;
  print "\n";

  print "### Ast";
  print spec.ast;
  print "\n";

  print "### Stdout";
  print spec.stdout_expect;
  print "\n";

  print "### Failure";
  (match spec.failure with
  | None -> ()
  | Some f -> string_of_failure f |> print);
  print "\n";

  Printf.printf "(2) Serializing %s..." path;
  let chan = Out_channel.create path in
  Out_channel.output_string chan "foo";
  Out_channel.close chan

let deserialize path =
  let chan = In_channel.create path in
  let lines = In_channel.input_lines chan in
  In_channel.close chan;
  let rec process_line state' lines acc =
    match lines with
    | line :: tail ->
        let r = Re.Pcre.regexp "^### ([a-zA-Z].*)" in
        let groups_opt = Re.exec_opt r line in
        let next_state, acc =
          match groups_opt with
          | Some groups -> (
              let title = Re.Group.get groups 1 in
              match state' with
              | NotParsing -> (Parsing (title, []), acc)
              | Parsing (prev_title, lines) ->
                  let forward_lines = List.rev lines in
                  (Parsing (title, []), (prev_title, forward_lines) :: acc))
          | None -> (
              match state' with
              | NotParsing -> (state', acc)
              | Parsing (prev_title, lines) ->
                  (Parsing (prev_title, line :: lines), acc))
        in
        (process_line [@tailcall]) next_state tail acc
    | [] -> (
        match state' with
        | NotParsing -> acc
        | Parsing (prev_title, lines) ->
            let forward_lines = List.rev lines in
            (prev_title, forward_lines) :: acc)
  in
  let parts = process_line NotParsing lines [] in
  let name_opt_ref = ref None in
  let ast_opt_ref = ref None in
  let program_opt_ref = ref None in
  let stdout_expect_opt_ref = ref None in
  let failure_opt_ref = ref None in
  List.iter parts ~f:(fun part ->
      let title, lines = part in
      let buf = Buffer.create 2 in
      List.iter lines ~f:(Buffer.add_string buf);
      let body = Buffer.contents buf in
      match title with
      | "Name" -> name_opt_ref := Some body
      | "Program" -> program_opt_ref := Some body
      | "Ast" -> ast_opt_ref := Some (String.strip body)
      | "Failure" ->
          let body = String.strip body in
          failure_opt_ref :=
            Some
              (match body with
              | "Parser_error" -> Parser_error
              | "Optimizer_error" -> Optimizer_error
              | "Runtime_error" -> Runtime_error
              | _ -> Printf.sprintf "Huh? %s" body |> failwith)
      | "Foo" (* No-op *) -> ()
      | _ -> Printf.sprintf "Huh? %s" title |> failwith);

  let ast = Option.value !ast_opt_ref ~default:"()" in
  {
    name = Option.value_exn !name_opt_ref;
    ast;
    program = Option.value_exn !program_opt_ref;
    stdout_expect = Option.value !stdout_expect_opt_ref ~default:"";
    failure = !failure_opt_ref;
  }
