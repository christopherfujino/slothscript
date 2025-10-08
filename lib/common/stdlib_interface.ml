(* TODO move arity here *)
type class_t = {
  name : string;
  members : string list;
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
        {
          name = "Directory";
          members = [ "exists"; "create"; "path" ];
          static_members = [];
        };
        {
          name = "File";
          members = [ "readString"; "openRead"; "openWrite" ];
          static_members = [ "new" ];
        };
        {
          (* Should we have separate types for reading and writing? *)
          name = "FileDescriptor";
          members = [ "close"; "read"; (* "write";*) "readAll"; "writeAll" ];
          static_members = [];
        };
        { name = "HashMap"; members = [ "merge" ]; static_members = [] };
        { name = "List"; members = [ "length" ]; static_members = [] };
        { name = "Number"; members = [ (* "floor"; *) ]; static_members = [] };
        {
          name = "Pipe";
          members = [ "read"; "write" ];
          static_members = [ "new" ];
        };
        { name = "Process"; members = []; static_members = [ "new" ] };
        { name = "ProcessHandle"; members = [ "wait" ]; static_members = [] };
        {
          name = "ProcessResult";
          members = [ "stderr"; "stdout" ];
          static_members = [];
        };
        { name = "String"; members = [ "trim" ]; static_members = [] };
      ];
  }
