open OUnit2
open Core

let pretty =
  [
    ( "escapes string literals with whitespace",
      fun _ ->
        let src = {|


func f() {
  42;
}

print("The answer is: ${f()}.")|} in

        let env =
          Compiler.Environment.create src |> Compiler.Environment.populate
        in
        let _, ast = Compiler.Main.parse env src in
        let ast_str = Compiler.Optimizer.prog_to_str ast in
        let pretty = Printer.sexp_formatter ast_str in
        let double_quote_count =
          String.fold pretty ~init:0 ~f:(fun i c ->
              if Char.( = ) c '"' then i + 1 else i)
        in
        assert_equal double_quote_count 2 );
  ]

let escape () =
  let open Interpreter.Common in
  let pp_diff formatter tuple =
    let left, right = tuple in
    let left = List.to_string ~f:Fun.id left in
    let right = List.to_string ~f:Fun.id right in
    Format.fprintf formatter "Actual: \"%s\"\nExpected: \"%s\"" left right
  in
  [
    ( "with no spaces, returns a single element",
      fun _ ->
        let input = "thisISaLongSTRING_without.anySPACES" in
        let output = shell_like_escape input in
        assert_equal ~pp_diff output [ input ] );
    ( "multiple elements output in the right order",
      fun _ ->
        let output = shell_like_escape "two words" in
        assert_equal ~pp_diff output [ "two"; "words" ] );
    ( "track double quotes",
      fun _ ->
        let input = "print \"Hello, world!\"" in
        let output = shell_like_escape input in
        assert_equal ~pp_diff output [ "print"; "Hello, world!" ] );
    ( "track single quotes",
      fun _ ->
        let input = "print 'Hello, world!'" in
        let output = shell_like_escape input in
        assert_equal ~pp_diff output [ "print"; "Hello, world!" ] );
    ( "nested quotes",
      fun _ ->
        let input = "'this should be \"only a single\" string'" in
        let output = shell_like_escape input in
        assert_equal ~pp_diff output
          [ "this should be \"only a single\" string" ] );
  ]

let compare_two_string_lists ?(prefix = "") interfaces implementations =
  List.iter interfaces ~f:(fun interface ->
      match List.find implementations ~f:(String.( = ) interface) with
      | None ->
          Printf.sprintf "You have not implemented %s%s" prefix interface
          |> assert_failure
      | Some _ -> ());

  if List.length interfaces = List.length implementations then ()
  else
    List.iter implementations ~f:(fun implementation ->
        match List.find interfaces ~f:(String.( = ) implementation) with
        | None ->
            Printf.sprintf
              "The implementation for %s%s is not defined as an interface"
              prefix implementation
            |> assert_failure
        | Some _ -> ())

let stdlib_interface_and_impl_match _ =
  let m = Interpreter.Native.make_test Interpreter.Mock_process.empty_spec in
  let module M = (val m) in
  let interface = Sloth_common.Stdlib_interface.globals in
  let impl_ids =
    Interpreter.Stdlib_impl.make_ids (module M)
    |> List.map ~f:(fun (name, _) -> name)
  in
  let impl_context_ids =
    Interpreter.Stdlib_impl.context_ids ~cwd:"/home/user"
      ~env:[| "USER=sloth" |] ~script_path:"/home/user/script.sloth" ~argv:[]
    |> List.map ~f:(fun (name, _) -> name)
  in
  compare_two_string_lists interface.ids impl_ids;
  compare_two_string_lists interface.context_ids impl_context_ids;

  let impl_protos = Interpreter.Stdlib_impl.make_protos (module M) in
  List.iter interface.protos ~f:(fun interface_proto ->
      let has_impl =
        List.fold impl_protos ~init:false
          ~f:(fun found { name; getters; setters; static_getters } ->
            if found then true
            else if String.(name = interface_proto.name) then (
              let impl_getters = List.map getters ~f:(fun (name, _) -> name) in
              let impl_setters = List.map setters ~f:(fun (name, _) -> name) in
              let prefix = Printf.sprintf "%s::" name in
              compare_two_string_lists ~prefix interface_proto.getters
                impl_getters;
              compare_two_string_lists ~prefix interface_proto.setters
                impl_setters;
              let impl_statics =
                List.map static_getters ~f:(fun (name, _) -> name)
              in
              compare_two_string_lists
                ~prefix:(Printf.sprintf "static %s." name)
                interface_proto.static_getters impl_statics;
              true)
            else false)
      in
      if not has_impl then
        Printf.sprintf
          "The interface prototype %s does not have an implementation"
          interface_proto.name
        |> assert_failure);
  if not (List.length interface.protos = List.length impl_protos) then
    assert_failure
      "You have more prototype implementations than interfaces TODO: message"

let get () =
  let f =
   fun tuple ->
    let name, callback = tuple in
    "[Unit test] " ^ name >:: callback
  in
  [
    "shell_like_escape" >::: List.map (escape ()) ~f;
    "pretty" >::: List.map pretty ~f;
    "STDLIB interface & implementation match"
    >:: stdlib_interface_and_impl_match;
  ]
