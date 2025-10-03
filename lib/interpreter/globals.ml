open Core

type t = {
  l : (module Native.Sig);
  identifiers : Runtime.t Identifiers.t;
  context_ids : Runtime.t Context.t;
  (* We need to store these at the top level so that we can find these for
   runtime lookup. They must also be stored in identifiers however, so users
   can invoke them explicitly. *)
  classes : Runtime.class_lookup;
  src : string;
  script_path : string;
  argv : string list;
}

(* TODO memoize *)
let make_globals m src script_path ~argv ~env =
  let module M = (val m : Native.Sig) in
  let classes = Hashtbl.create (module String) in
  let identifiers = Identifiers.create () in
  let context_ids = Context.create () in
  let make_method name arity methods
      (cb :
        Runtime.t Context.t ->
        Runtime.t list ->
        (Runtime.t, Runtime.breaking_type) Either.t) =
    let cb =
     fun ctx args ->
      let arg_len = List.length args in
      if not (Int.equal arg_len arity) then
        Second
          (Runtime.create_error
             (Printf.sprintf
                "You passed %d arguments (%s) to %s but %d were expected"
                arg_len
                (List.fold_left args ~init:"" ~f:(fun msg arg ->
                     msg ^ Runtime.to_s arg ^ ", "))
                name arity))
      else cb ctx args
    in
    let native = Runtime.Native { cb } in
    Hashtbl.add_exn methods ~key:name ~data:(Runtime.Func native)
  in
  let make_func name ?arity identifiers cb =
    let (cb
          : Runtime.t Context.t ->
            Runtime.t list ->
            (Runtime.t, Runtime.breaking_type) Either.t) =
     fun ctx args ->
      let arg_len = List.length args in
      if
        Option.is_some arity && not (Int.equal arg_len (Option.value_exn arity))
      then
        Second
          (Runtime.create_error
          @@ Printf.sprintf "You passed %d arguments but %d were expected"
               arg_len
          @@ Option.value_exn arity)
      else cb ctx args
    in
    Identifiers.bind identifiers name (Runtime.Func (Native { cb }))
    |> Option.value_exn
         ~message:
           (Printf.sprintf "Failed to bind the function named %s at %s" name
              __LOC__)
  in
  List.iter (Stdlib_impl.make_ids m) ~f:(fun impl ->
      make_func impl.name ?arity:impl.arity identifiers impl.cb);

  (* TODO inject cwd *)
  List.iter
    (Stdlib_impl.context_ids ~cwd:(Sys_unix.getcwd ()) ~env ~script_path ~argv)
    ~f:(fun (name, t) ->
      Context.bind context_ids name t |> Option.value_exn ~message:__LOC__);

  List.iter (Stdlib_impl.make_protos m)
    ~f:(fun { name; methods; static_members } ->
      let cl =
        Runtime.
          {
            instance_members = Hashtbl.create (module String);
            static_members = Hashtbl.create (module String);
          }
      in
      List.iter methods ~f:(fun meth ->
          make_method meth.name
            (Option.value_exn meth.arity)
            cl.instance_members meth.cb);

      List.iter static_members ~f:(fun meth ->
          make_method meth.name
            (Option.value_exn meth.arity)
            cl.static_members meth.cb);
      (match Hashtbl.add classes ~key:name ~data:cl with
      | `Duplicate ->
          Printf.sprintf "Tried to implement the class %s twice" name
          |> failwith
      | `Ok -> ());
      (* Bind at the root scope so users can reach it from IdRefs *)
      Identifiers.bind identifiers name (Runtime.Prototype { name })
      |> Option.value_exn);

  { l = m; identifiers; src; classes; context_ids; script_path; argv }
