open Core

type t = {
  l : (module Native_sig.Sig);
  identifiers : Runtime.t Identifiers.t;
  context_ids : Runtime.t Context.t;
  (* We need to store these at the top level so that we can find these for
   runtime lookup. They must also be stored in identifiers however, so users
   can invoke them explicitly. *)
  classes : Runtime.class_lookup;
  src : string;
  script_path : string;
  argv : string list;
  stack_frames : Runtime.backtrace;
  current_function_name : string;
      (* Store this since stack_frames stores the name of the enclosing func *)
  version : Sloth_common.Semver.t;
}

let push_frame t next_func pos =
  {
    t with
    stack_frames = (t.current_function_name, pos) :: t.stack_frames;
    current_function_name = next_func;
  }

(* TODO memoize *)
let make_globals m src script_path ~argv ~env ~version =
  let module M = (val m : Native_sig.Sig) in
  let classes = Hashtbl.create (module String) in
  let identifiers : Runtime.t Identifiers.t = Identifiers.create () in
  let context_ids = Context.create () in
  List.iter (Stdlib_impl.make_ids m) ~f:(fun (name, v) ->
      Identifiers.bind identifiers name v
      |> Option.value_exn
           ~message:
             (Printf.sprintf "Failed to bind the function named %s at %s" name
                __LOC__));

  (* TODO inject cwd *)
  List.iter
    (Stdlib_impl.context_ids ~cwd:(Sys_unix.getcwd ()) ~env ~script_path ~argv
       ~version)
    ~f:(fun (name, t) ->
      Context.bind context_ids name t |> Option.value_exn ~message:__LOC__);

  List.iter (Stdlib_impl.make_protos m)
    ~f:(fun { name; getters; setters; static_getters } ->
      let cl =
        Runtime.
          {
            instance_getters = Hashtbl.create (module String);
            instance_setters = Hashtbl.create (module String);
            static_getters = Hashtbl.create (module String);
          }
      in
      List.iter getters ~f:(fun (name, getter) ->
          Hashtbl.add_exn cl.instance_getters ~key:name ~data:getter);

      List.iter setters ~f:(fun (name, setter) ->
          Hashtbl.add_exn cl.instance_setters ~key:name ~data:setter);

      List.iter static_getters ~f:(fun (name, getter) ->
          Hashtbl.add_exn cl.static_getters ~key:name ~data:getter);

      (match Hashtbl.add classes ~key:name ~data:cl with
      | `Duplicate ->
          Printf.sprintf "Tried to implement the class %s twice" name
          |> failwith
      | `Ok -> ());
      (* Bind at the root scope so users can reach it from IdRefs *)
      Identifiers.bind identifiers name (Runtime.Prototype { name })
      |> Option.value_exn);

  {
    l = m;
    identifiers;
    src;
    classes;
    context_ids;
    script_path;
    argv;
    stack_frames = [];
    current_function_name = "(top-level)";
    version;
  }
