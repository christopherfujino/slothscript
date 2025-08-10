open Core

type t = {
  l : (module Sloth_stdlib.StdlibSig);
  identifiers : Runtime.t Identifiers.t;
  classes : Runtime.class_lookup;
  src : string;
}

let make_ctx m src =
  let module M = (val m : Sloth_stdlib.StdlibSig) in
  let make_func name arity identifiers cb =
    let unit_opt =
      Identifiers.bind identifiers name
        (Runtime.Func
           (Native
              {
                parameters = [ "value" ];
                cb =
                  (fun args ->
                    if not (Int.equal (List.length args) arity) then
                      failwith
                        "You passed the wrong number of arguments to print()";
                    cb args);
                identifiers;
              }))
    in
    Option.value_exn unit_opt
  in
  let classes = Hashtbl.create (module String) in
  let identifiers = Identifiers.create () in
  List.iter Sloth_common.Stdlib_interface.globals ~f:(fun (name, _) ->
      match name with
      | "print" ->
          make_func "print" 1 identifiers (fun args ->
              let arg = List.hd_exn args in
              Runtime.to_s arg |> M.print_s;
              M.print_s "\n";
              Runtime.Null)
      | "$cwd" ->
          Identifiers.bind identifiers "$cwd" (Runtime.String "TODO")
          |> Option.value_exn
      | "Number" -> failwith "TODO"
      | _ ->
          Printf.sprintf "TODO: You have not yet implemented %s" name
          |> failwith);
  { l = m; identifiers; src; classes }
