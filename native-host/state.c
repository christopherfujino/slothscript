#include "state.h"
#include <caml/alloc.h>    // caml_copy_nativeint()
#include <caml/mlvalues.h> // value
#include <stdint.h>        // uint8_t
#include <stdlib.h>        // malloc()

/** 16mb. */
static const size_t MAX_CAP = 16 * 1024 * 1024;
static const size_t INITIAL_CAP = (1 << 4) - 1;

State state_new() {
  return (State){
      .len = 0,
      .cap = INITIAL_CAP,
      .instructions = (uint8_t *)malloc(INITIAL_CAP * sizeof(uint8_t)),
  };
}

value state_wrap(State *state) {
  // This is boxed
  // TODO: make this unboxed
  return caml_copy_nativeint((long)state);
}

value state_add(size_t s, value byte_value) {
  State *p = (State *)Nativeint_val(s);
  if (p->len == p->cap) {
    // TODO: grow
    const size_t next_cap = ((size_t)p->cap + 1) * 2 - 1;
    printf("[DEBUG] growing from %ld -> %ld (0x%04lX)\n", p->cap, next_cap,
           next_cap);
    if (next_cap > MAX_CAP) {
      fprintf(stderr, "Tried to grow State beyond its upper max!\n");
      abort();
    }
    void *next_buffer = realloc(p->instructions, next_cap);
    if (next_buffer == NULL) {
      fprintf(stderr, "Failure in realloc()\n");
      abort();
    }
    p->instructions = next_buffer;
    p->cap = next_cap;
  }

  // this should be an unboxed 63/64 bit int
  long l = Long_val(byte_value);
  if (l > UCHAR_MAX) {
    fprintf(stderr, "state_add() passed invalid byte value = %ld (0x%02lX)\n",
            l, l);
    abort();
  }
  p->instructions[p->len] = (uint8_t)l;
  p->len += 1;

  return Val_unit;
}
