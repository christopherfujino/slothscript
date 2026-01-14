#include <caml/alloc.h>
#include <caml/mlvalues.h>

#include "_native.hpp"

extern "C" {

value file_read_all(value path) {
  value contents = _file_read_all(String_val(path));
  return contents;
}

} // extern "C"
