#include "caml/alloc.h"
#include "caml/mlvalues.h"
#include <cstdio>

// Warning: this assumes the file is text, with no NULL bytes in it.
// TODO: make this return an option
value _file_read_all(const char *path) {
  FILE *file = fopen(path, "r");

  if (file == nullptr) {
    perror("TODO: make this return none; you tried to read a file that does not exist");
    exit(1);
  }

  size_t len;
  {
    fseek(file, 0, SEEK_END);
    len = ftell(file);
    rewind(file);
  }

  // Add 1 for trailing '\0'
  auto ocaml_string = caml_alloc_string(len + 1);
  char *buffer = Bp_val(ocaml_string);

  auto bytes_read = fread(buffer, sizeof(char), len, file);
  if (bytes_read != len) {
    fprintf(stderr, "Native C error: len = %ld; read = %ld\n%s:%d\n", len, bytes_read, __FILE__, __LINE__);
    exit(1);
  }
  buffer[len] = '\0';

  return ocaml_string;
}
