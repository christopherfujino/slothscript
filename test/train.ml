open Common
open Core

let errors = ref []

let ast_needs_update spec canonical_ast =
  let pretty_ast = Printer.sexp_formatter spec.ast in
  if String.equal canonical_ast spec.ast then []
  else if String.equal canonical_ast pretty_ast then [ "Wrong AST" ]
  else [ "Ugly AST" ]

let process_spec_needs_update spec pretty_proc_spec =
  if String.equal pretty_proc_spec spec.proc_spec then []
  else [ "Ugly proc_spec" ]

let f spec_dir i arg =
  if i = 0 then
    (* args[0] is the bin name *)
    ()
  else
    let src_path = Printf.sprintf "%s/%s" spec_dir arg in
    let spec = Spec_parser.deserialize arg in
    let pretty_proc_spec = Printer.sexp_formatter spec.proc_spec in
    let open Option.Monad_infix in
    (try
       let _, prog =
         (Compiler.Environment.create spec.program
         |> Compiler.Environment.populate |> Compiler.Main.parse)
         @@ spec.program
       in
       Some prog
     with exn ->
       errors := (arg, exn) :: !errors;
       None)
    >>| (fun prog ->
    let canonical_ast =
      Compiler.Optimizer.prog_to_str prog |> Printer.sexp_formatter
    in

    let reasons =
      List.append
        (ast_needs_update spec canonical_ast)
        (process_spec_needs_update spec pretty_proc_spec)
    in
    match List.length reasons with
    | 0 -> ()
    | 1 ->
        let s = List.hd_exn reasons in
        Printf.printf "[%s]\t%s" s arg;
        Spec_parser.serialize src_path
          { spec with ast = canonical_ast; proc_spec = pretty_proc_spec }
    | 2 ->
        let s1 = List.nth_exn reasons 0 in
        let s2 = List.nth_exn reasons 1 in
        Printf.printf "[%s, %s]\t%s" s1 s2 arg;
        Spec_parser.serialize src_path
          { spec with ast = canonical_ast; proc_spec = pretty_proc_spec }
    | _ -> failwith "Unreachable")
    |> Option.value ~default:()

let () =
  let spec_dir =
    match Sys.getenv "TEST_DIR" with
    | None -> failwith "You must set the TEST_DIR env var"
    | Some d -> d
  in
  let args = Sys.get_argv () in
  if Array.length args = 1 then failwith "Usage: train.exe [SPEC_PATH]+";
  Array.iteri args ~f:(f spec_dir);
  (* TODO check errors *)
  let errors = List.rev !errors in
  if List.length errors > 0 then
    List.iter errors ~f:(fun (name, e) ->
        match e with
        | Compiler.Common.ParserFailure msg ->
            Printf.printf "[Bad] %s - %s" name msg
        | _ -> raise e)
