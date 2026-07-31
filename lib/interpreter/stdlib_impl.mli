type proto_t = {
  name : string;
  getters : (string * (Runtime.t -> (Runtime.t, string) Result.t)) list;
  setters : (string * (Runtime.t -> Runtime.t -> (unit, string) Result.t)) list;
  static_getters : (string * (Runtime.t -> (Runtime.t, string) Result.t)) list;
}

val make_ids : (module Native_sig.Sig) -> (string * Runtime.t) list
val make_protos : (module Native_sig.Sig) -> proto_t list

val context_ids :
  cwd:string ->
  env:string array ->
  script_path:string ->
  argv:string list ->
  version:Sloth_common.Semver.t ->
  (string * Runtime.t) list
