#pragma once

#include "caml/alloc.h"
#include "caml/mlvalues.h"
#include <cerrno>
#include <cstdio>
#include <cstring>
#include <unistd.h>

inline value _file_read_all(const char *path) {
  FILE *file = fopen(path, "r");

  if (file == nullptr) {
    // None, first constant constructor = 0
    return Val_int(0);
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
    // None, first constant constructor = 0
    return Val_int(0);
  }
  buffer[len] = '\0';

  // Some, first non-constant constructor = 0
  return caml_alloc_1(0, ocaml_string);
}

inline value __chdir(const char *newpath) {
  int retval = chdir(newpath);
  if (retval == 0) {
    // Ok, first non-constant constructor = 0
    return caml_alloc_1(0, Val_unit);
  }

  return caml_alloc_1(1, caml_copy_string(strerror(errno)));
}
