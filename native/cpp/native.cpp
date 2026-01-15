#include <caml/alloc.h>
#include <caml/mlvalues.h>

#include "_native.hpp"

extern "C" {

// string -> string option
value file_read_all(value path) {
  return _file_read_all(String_val(path));
}

// string -> (unit, string) Result.t
value _chdir(value newpath) {
  return __chdir(String_val(newpath));
}

} // extern "C"
