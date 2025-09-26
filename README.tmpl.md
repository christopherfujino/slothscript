# Slothscript

A slow scripting language.

```sloth
{{ .test_green_specs_fibonacci_sloth }}
```

## Installation

Note that:

1. This product is licensed under the [BSD-3 license](./LICENSE), meaning that
there is no warranty provided for it. Use at your own risk.
1. Sloth is currently in pre-alpha state, meaning it is not expected to be
generally useful.
1. The *goal* of the project is to only support Unix-like systems (currently
only Linux is tested). Windows support is an explicit *non-goal*.
1. There will not be pre-compiled builds of the interpreter until a future
release. Compiling the interpreter will require an Ocaml toolchain.

## Tour

User-written variables or functions must start with a lowercase letter or `_`.
Names starting with a capital letter are reserved for class names.

```sloth
{{ .test_green_specs_var_reference_sloth }}
```

First-class functions:

```sloth
{{ .test_green_specs_first_class_func_sloth }}
```

## Language

### Reserved words

- `let` - variable binding keyword; `let x = 1`
- `func` - function declaration keyword, can be used either as a top-level declaration or an anonymous function expression; `let increment = func(v) { v + 1; }`
- `for` - keyword for declaring loops, can be used either in C-style loops (`for let i = 0; i < MAX; i = i + 1 {}`) or for-in loops over lists (`for user in [user1, user2, user3] {}`)
- `in` - keyword for use in for-in loops
- `if` - conditional branching keyword
- `else` - conditional branching keyword
- `do` - keyword for do-block expressions; `let s = do {let s = "init ${getString()}"; s = "${s}${getString()}"; s}`
- `return` - early return from a function block
- `break` - early return from a loop
- `continue` - skip to the next iteration of a loop
- `with` - bind new (dynamically-scoped) context variables
- `true` - `Bool` literal
- `false` - `Bool` literal
- `null` - `Null` literal singleton
- `not` - prefix logical NOT operator; `assert(not false)`

### Context Variables

{{ .docs_context_md }}
