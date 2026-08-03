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

const char program[] = "'uname'!\n\
print(\"Hello, world from Sloth!\")\n\
print($argv)\n\
";

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
    fprintf(stderr,
            "Failed to lookup OCaml value named \"init_repl_env\"\n");
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

int main(int _, char **argv) {
  if (isatty(STDIN_FILENO)) {
    repl(argv);
  } else {
    caml_startup(argv);
    State state = state_new();

    const value *compiler = caml_named_value("parse_compile_interpret");
    if (compiler == NULL) {
      fprintf(
          stderr,
          "Failed to lookup OCaml value named \"parse_compile_interpret\"\n");
      abort();
    }

    value tuple = caml_alloc_tuple(2);
    {
      value program_val =
          caml_alloc_initialized_string(strlen(program), program);
      Store_field(tuple, 0, program_val);
      Store_field(tuple, 1, state_wrap(&state));
    }

    caml_result_unwrap(caml_callback_res(*compiler, tuple));
    caml_shutdown();
  }
}
