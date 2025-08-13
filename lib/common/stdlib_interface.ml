type class_t = { name : string; methods : string list }
type t = { ids : string list; protos : class_t list }

let globals =
  {
    ids = [ "print"; "assert"; "$cwd" ];
    protos =
      [
        { name = "Process"; methods = [] };
        {
          name = "Number";
          methods = [ "+"; "-"; "/"; "*"; "<"; ">"; "<="; ">=" ];
        };
      ];
  }
