#include "state.h"

#include <caml/alloc.h>    // caml_alloc_initialized_string()
#include <caml/callback.h> // caml_startup(), caml_callback_res()
#include <caml/mlvalues.h> // value
#include <caml/printexc.h> // caml_format_exception()
#include <stdint.h>        // uint8_t
#include <stdlib.h>        // abort(), malloc(), realloc()
#include <string.h>        // strlen()
#include <unistd.h>        // isatty()

// TODO: this is sus
extern void caml_print_exception_backtrace(void);

static value caml_result_unwrap(caml_result result) {
  if (!caml_result_is_exception(result)) {
    return result.data;
  }
  {
    char *exn_msg = caml_format_exception(result.data);
    fprintf(stderr, "\nOCaml raised: %s\n", exn_msg);
    caml_stat_free(exn_msg);
  }
  caml_print_exception_backtrace();
  fflush(stderr);
  abort();
}

void repl(char **argv) {
  caml_startup(argv);

  const value *init = caml_named_value("init_repl_env");
  if (init == NULL) {
    fprintf(stderr, "Failed to lookup OCaml value named \"init_repl_env\"\n");
    abort();
  }
  value env = caml_result_unwrap(caml_callback_res(*init, Val_unit));

  const value *interpreter = caml_named_value("parse_compile_interpret_print");
  if (interpreter == NULL) {
    fprintf(stderr,
            "Failed to lookup OCaml value named \"parse_compile_interpret\"\n");
    abort();
  }
  char line[BUFSIZ];

  while (1) {
    // TODO use a line editor
    fputs("> ", stdout);
    char *p = fgets(line, BUFSIZ, stdin);
    if (p == NULL) {
      fputc('\n', stdout);
      goto cleanup;
    }

    value tuple = caml_alloc_tuple(2);
    {
      value program_val = caml_alloc_initialized_string(strlen(line), line);
      Store_field(tuple, 0, env);
      Store_field(tuple, 1, program_val);
    }

    env = caml_result_unwrap(caml_callback_res(*interpreter, tuple));
  }

cleanup:
  caml_shutdown();
}

void interpreter(char **argv, char *program, size_t program_len) {
  caml_startup(argv);
  State state = state_new();

  const value *compiler = caml_named_value("parse_compile_interpret");
  if (compiler == NULL) {
    fprintf(stderr,
            "Failed to lookup OCaml value named \"parse_compile_interpret\"\n");
    abort();
  }

  value tuple = caml_alloc_tuple(2);
  {
    // TODO: unneccessary copy
    value program_val = caml_alloc_initialized_string(program_len, program);
    Store_field(tuple, 0, program_val);
    Store_field(tuple, 1, state_wrap(&state));
  }

  caml_result_unwrap(caml_callback_res(*compiler, tuple));
  caml_shutdown();
}

int main(int argc, char **argv) {
  if (argc == 1) {
    if (isatty(STDIN_FILENO)) {
      repl(argv);
    } else {
      fprintf(stderr, "TODO! %d\n", __LINE__);
      abort();
      interpreter(argv, "TODO", 4);
    }
  } else if (argc > 1) {
    char *script = argv[1];
    FILE *file = fopen(script, "r");
    if (file == NULL) {
      fprintf(stderr, "Failed to open %s ", script);
      perror("with");
      abort();
    }

    // Let's find the length of the file before reading
    size_t len;
    {
      fseek(file, 0, SEEK_END);
      len = ftell(file);
      rewind(file);
    }

    // +1 for '\0'
    char *buffer = (char *)malloc(len + 1);
    size_t n = fread(buffer, sizeof(char), len, file);
    if (n != len) {
      // TODO: make correct
      fprintf(stderr, "%ld != %ld\n", n, len);
      abort();
    }
    buffer[len] = '\0';

    interpreter(argv, buffer, len);
  } else {
    fprintf(stderr, "Unreachable!\n");
    abort();
  }
}
