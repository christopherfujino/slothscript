(* TODO move arity here *)
type class_t = {
  name : string;
  methods : string list;
  static_members : string list;
}

type t = { ids : string list; context_ids : string list; protos : class_t list }

let globals =
  {
    ids = [ "print"; "assert" ];
    context_ids = [ "$cwd"; "$scriptDir"; "$script" ];
    protos =
      [
        { name = "Process"; methods = []; static_members = [ "new" ] };
        { name = "ProcessResult"; methods = [ "stdout" ]; static_members = [] };
        { name = "Number"; methods = []; static_members = [] };
        {
          name = "File";
          methods = [ "readString" ];
          static_members = [ "new" ];
        };
        {
          name = "Directory";
          methods = [ "exists"; "create"; "path" ];
          static_members = [];
        };
        { name = "String"; methods = [ "trim" ]; static_members = [] };
      ];
  }
