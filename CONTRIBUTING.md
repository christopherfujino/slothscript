To debug the project:

```sh
# build bytecode version of pure OCaml interpreter
dune build bin/main.bc
ocamldebug ./_build/default/bin/main.bc
```
