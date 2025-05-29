.PHONY: test
test:
	# A synonym for dune runtest
	# OUNIT_CI=true makes output prettier
	OUNIT_CI=true dune build @runtest --force

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

.PHONY: build
build:
	dune build

.PHONY: get
get:
	opam install . --deps-only --with-test --with-doc -vv

.PHONY: list-errors
list-errors:
	menhir --list-errors lib/compiler/parser.mly

.PHONY: ci
ci:
	dune exec tool/ci.exe

.PHONY: todos
todos:
	grep -rnI TODO | grep -v '^_build\/'| fgrep -v ignorethisparticularline

.PHONY: clean
clean:
	dune clean
