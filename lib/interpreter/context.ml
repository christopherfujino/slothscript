open Core

type t = {
  l : (module Sloth_stdlib.StdlibSig);
  identifiers : Runtime.t Identifiers.t;
  classes : Runtime.class_lookup;
  src : string;
}

let make_ctx m src =
  let module M = (val m : Sloth_stdlib.StdlibSig) in
  let classes = Hashtbl.create (module String) in
  let identifiers = Identifiers.create () in
  let make_method arity methods cb =
    Hashtbl.add_exn methods ~key:"+"
      ~data:
        (Runtime.Native
           {
             parameters = [ "this"; "left"; "right" ];
             cb =
               (fun args ->
                 let arg_len = List.length args in
                 if not (Int.equal arg_len arity) then
                   Error
                     (Printf.sprintf
                        "You passed %d arguments (%s) but %d were expected"
                        arg_len
                        (List.fold_left args ~init:"" ~f:(fun msg arg ->
                             msg ^ Runtime.to_s arg ^ ", "))
                        arity)
                 else Ok (cb args));
             identifiers;
           })
  in
  let make_func name arity identifiers cb =
    Identifiers.bind identifiers name
      (Runtime.Func
         (Native
            {
              parameters = [ "value" ];
              cb =
                (fun args ->
                  if not (Int.equal (List.length args) arity) then
                    Error "You passed the wrong number of arguments to TODO"
                  else Ok (cb args));
              identifiers;
            }))
    |> Option.value_exn
  in
  List.iter Sloth_common.Stdlib_interface.globals ~f:(fun (name, variant) ->
      match variant with
      | Value -> (
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
          | _ ->
              Printf.sprintf "TODO: You have not yet implemented %s" name
              |> failwith)
      | Class { properties; methods } -> (
          let cl = Runtime.{ methods = Hashtbl.create (module String) } in
          match name with
          | "Number" ->
              List.iter properties ~f:(fun _ -> failwith "TODO properties");
              List.iter methods ~f:(function
                | "+" ->
                    make_method 2 cl.methods (fun args ->
                        let lhs =
                          List.nth_exn args 0 |> Runtime.num_of_val
                          |> Option.value_exn
                        in
                        let rhs =
                          List.nth_exn args 1 |> Runtime.num_of_val
                          |> Option.value_exn
                        in
                        Runtime.Num (lhs +. rhs))
                | _ -> failwith "Unimplemented");
              Hashtbl.add_exn classes ~key:"Number" ~data:cl
          | _ -> failwith "TODO"));
  { l = m; identifiers; src; classes }
