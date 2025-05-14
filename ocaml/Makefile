.PHONY: stdlib_test
stdlib_test:
	dune exec ocaml_stdlib

.PHONY: interpreter
interpreter:
	dune exec sloth_script

.PHONY: repl
repl: build
	# opam install utop
	dune utop lib/

.PHONY: test
test: 

.PHONY: build
build:
	dune build
