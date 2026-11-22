#include <caml/alloc.h>    // caml_alloc_initialized_string()
#include <caml/callback.h> // caml_startup(), caml_callback_res()
#include <caml/mlvalues.h> // value
#include <caml/printexc.h> // caml_format_exception()
#include <limits.h>        // PATH_MAX
#include <stdint.h>        // uint8_t
#include <stdlib.h>        // abort(), malloc(), realloc(), realpath()
#include <string.h>        // strlen()
#include <unistd.h>        // getcwd(), isatty()

#include "editline.h"
#include "state.h"

// TODO: this is sus
extern void caml_print_exception_backtrace(void);

static value caml_result_unwrap(caml_result result) {
  if (!caml_result_is_exception(result)) {
    return result.data;
  }
  {
    char *exn_msg = caml_format_exception(result.data);
    fprintf(stderr, "%s\n", exn_msg);
    caml_stat_free(exn_msg);
  }
  caml_print_exception_backtrace();
  fflush(stderr);
  exit(1);
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
  char *line = nullptr;
  value tuple = caml_alloc_tuple(2);

  while ((line = readline("> ")) != nullptr) {
    value program_val = caml_alloc_initialized_string(strlen(line), line);
    Store_field(tuple, 0, env);
    Store_field(tuple, 1, program_val);
    env = caml_result_unwrap(caml_callback_res(*interpreter, tuple));
  }
  printf("\n");

  caml_shutdown();
}

long interpreter(char **argv, size_t program_len,
                 const char program[static program_len],
                 const char *relative_program_path) {
  long code = -1;
  caml_startup(argv);
  {
    State state = state_new();

    const value *compiler = caml_named_value("parse_compile_interpret");
    if (compiler == NULL) {
      fprintf(
          stderr,
          "Failed to lookup OCaml value named \"parse_compile_interpret\"\n");
      abort();
    }

    value tuple = caml_alloc_tuple(3);
    {
      char *program_path = nullptr;
      program_path = realpath(relative_program_path, program_path);
      if (program_path == nullptr) {
        perror("realpath() failed");
        abort();
      }
      // TODO: unneccessary copy
      value program_val = caml_alloc_initialized_string(program_len, program);
      value program_path_val =
          caml_alloc_initialized_string(strlen(program_path), program_path);
      Store_field(tuple, 0, program_val);
      Store_field(tuple, 1, state_wrap(&state));
      Store_field(tuple, 2, program_path_val);
    }

    value code_value = caml_result_unwrap(caml_callback_res(*compiler, tuple));
    code = Long_val(code_value);
  }
  caml_shutdown();
  return code;
}

int main(int argc, char **argv) {
  if (argc == 1) {
    if (isatty(STDIN_FILENO)) {
      repl(argv);
    } else {
      constexpr size_t buflen = BUFSIZ;
      char buffer[buflen + 1] = {};
      char *program = nullptr;
      size_t program_len = 0;
      while (1) {
        // important to read buflen number of bytes, so bytes read is accurate
        size_t bytes_read = fread(buffer, 1, buflen, stdin);
        if (bytes_read < buflen) {
          if (feof(stdin)) {
            program =
                realloc(program, (program_len + buflen + 1) * sizeof(char));
            if (program == nullptr) {
              perror("realloc() failed");
              return 1;
            }
            strncpy(program + program_len, buffer, bytes_read);
            program_len += bytes_read;
            program[program_len] = '\0';
            break;
          }
          fprintf(stderr, "Error while trying to read from STDIN!\n");
          return 1;
        }
        program = realloc(program, program_len + buflen);
        if (program == nullptr) {
          perror("realloc() failed");
          return 1;
        }
        memcpy(program + program_len, buffer, buflen);
        program_len += buflen;
      }
      // 6 is the length of "/stdin"
      char *cwd = malloc(PATH_MAX + 6);
      cwd = getcwd(cwd, PATH_MAX);
      if (cwd == nullptr) {
        fprintf(stderr, "getcwd() failed\n");
        return 1;
      }
      strcat(cwd, "/stdin");
      long code = interpreter(argv, program_len, program, cwd);
      free(cwd);
      free(program);
      return code;
    }
  } else if (argc > 1) {
    if (argc > 2) {
      fprintf(stderr, "TODO: implement more robust arg-parsing\n");
      return 1;
    }
    char *script = argv[1];
    FILE *file = fopen(script, "r");
    if (file == NULL) {
      fprintf(stderr, "Failed to open %s ", script);
      perror("fopen() failed");
      return 1;
    }

    // Let's find the length of the file before reading
    size_t len;
    {
      fseek(file, 0, SEEK_END);
      len = ftell(file);
      rewind(file);
    }

    // +1 for '\0'
    char *program = (char *)calloc(len + 1, 1);
    // important to read buflen number of bytes, so bytes read is accurate
    size_t n = fread(program, sizeof(char), len, file);
    if (n != len) {
      // TODO: make correct
      fprintf(stderr, "fread() failed: %ld != %ld\n", n, len);
      abort();
    }
    program[len] = '\0';

    interpreter(argv, len, program, script);
  } else {
    fprintf(stderr, "Unreachable!\n");
    abort();
  }
}
