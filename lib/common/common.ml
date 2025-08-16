exception InternalFailure of string

let internal_failure loc =
  raise @@ InternalFailure (Printf.sprintf "Internal failure at %s" loc)
