.PHONY: test
test:
	# A synonym for dune runtest
	dune build @runtest --force

.PHONY: stdlib_test
stdlib_test:
	dune exec ocaml_stdlib

.PHONY: repl
repl:
	dune exec repl

.PHONY: utop
utop: build
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
