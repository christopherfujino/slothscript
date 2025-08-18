(* TODO move arity here *)
type class_t = {
  name : string;
  methods : string list;
  static_members : string list;
}

type t = { ids : string list; protos : class_t list }

let globals =
  {
    ids = [ "print"; "assert"; "$cwd" ];
    protos =
      [
        { name = "Process"; methods = []; static_members = [ "new" ] };
        (*{ name = "ProcessResult"; methods = []; static_members = [] }; *)
        { name = "Number"; methods = []; static_members = [] };
        {
          name = "File";
          methods = [ "readString" ];
          static_members = [ "new" ];
        };
      ];
  }
