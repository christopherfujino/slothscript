(* TODO move arity here *)
type class_t = {
  name : string;
  methods : string list;
  static_members : string list;
}

type t = { ids : string list; context_ids : string list; protos : class_t list }

let globals =
  {
    ids = [ "assert"; "exit"; "print" ];
    context_ids =
      [
        "$argv";
        "$cwd";
        "$env";
        "$script";
        "$scriptDir";
        "$stderr";
        "$stdin";
        "$stdout";
      ];
    protos =
      [
        { name = "List"; methods = [ "length" ]; static_members = [] };
        { name = "Process"; methods = []; static_members = [ "new" ] };
        { name = "ProcessHandle"; methods = [ "wait" ]; static_members = [] };
        { name = "ProcessResult"; methods = [ "stdout" ]; static_members = [] };
        { name = "Number"; methods = []; static_members = [] };
        {
          name = "File";
          methods = [ "readString"; "openRead"; "openWrite" ];
          static_members = [ "new" ];
        };
        {
          (* Should we have separate types for reading and writing? *)
          name = "FileDescriptor";
          methods = [ "readAll" ];
          static_members = [];
        };
        {
          name = "Directory";
          methods = [ "exists"; "create"; "path" ];
          static_members = [];
        };
        { name = "String"; methods = [ "trim" ]; static_members = [] };
        { name = "HashMap"; methods = [ "merge" ]; static_members = [] };
      ];
  }
