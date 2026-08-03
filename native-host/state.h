#ifndef __SLOTHSCRIPT_NATIVE_HOST_STATE_H
#define __SLOTHSCRIPT_NATIVE_HOST_STATE_H

#include <caml/mlvalues.h> // value
#include <stddef.h>        // size_t
#include <stdint.h>        // uint8_t

/** Shared state between compiler and interpreter. */
typedef struct State {
  size_t len;
  size_t cap;
  uint8_t *instructions;
} State;

State state_new();
value state_wrap(State *state);

#endif // __SLOTHSCRIPT_NATIVE_HOST_STATE_H
