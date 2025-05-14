.PHONY: test
test:
	dune runtest

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

.PHONY: get
get:
	opam install . --deps-only --with-test --with-doc -vv

.PHONY: list-errors
list-errors:
	menhir --list-errors lib/parser.mly
