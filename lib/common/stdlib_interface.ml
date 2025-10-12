(* TODO move arity here *)
type class_t = {
  name : string;
  getters : string list;
  setters : string list;
  static_getters : string list;
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
          getters = [ "exists"; "create"; "path" ];
          setters = [];
          static_getters = [];
        };
        {
          name = "File";
          getters = [ "readString"; "openRead"; "openWrite" ];
          setters = [];
          static_getters = [ "new" ];
        };
        {
          (* Should we have separate types for reading and writing? *)
          name = "FileDescriptor";
          getters = [ "close"; "read"; "write"; "readAll"; "writeAll" ];
          setters = [];
          static_getters = [];
        };
        {
          name = "HashMap";
          getters = [ "merge" ];
          setters = [];
          static_getters = [];
        };
        {
          name = "List";
          getters = [ "contains"; "forEach"; "length"; "push" ];
          setters = [];
          static_getters = [];
        };
        {
          name = "Number";
          getters = [ (* "floor"; *) ];
          setters = [];
          static_getters = [];
        };
        {
          name = "Pipe";
          getters = [ "read"; "write" ];
          setters = [];
          static_getters = [ "new" ];
        };
        {
          name = "Process";
          getters =
            [ "blockBuffer"; "blockInherit"; "forkBuffer"; "stderr"; "stdout" ];
          setters = [ "stderr"; "stdout" ];
          static_getters = [ "new" ];
        };
        {
          name = "ProcessHandle";
          getters = [ "wait" ];
          setters = [];
          static_getters = [];
        };
        {
          name = "ProcessResult";
          getters = [ "stderr"; "stdout" ];
          setters = [];
          static_getters = [];
        };
        {
          name = "String";
          getters = [ "split"; "trim"; "contains" ];
          setters = [];
          static_getters = [];
        };
      ];
  }
