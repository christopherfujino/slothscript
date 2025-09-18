# Slothscript

A slow scripting language.

```sloth
{{ .test_green_specs_fibonacci_sloth }}
```

## Dependencies

Only tested on Linux, should support macOS, but Windows is explicitly not
supported.

The runtime dependency is `readline(3)`.

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
- `true` - `Bool` literal
- `false` - `Bool` literal
- `null` - `Null` literal singleton
- `not` - prefix logical NOT operator; `assert(not false)`
- `with` - bind new (dynamically-scoped) context variables

### Context Variables

{{ .docs_context_md }}
