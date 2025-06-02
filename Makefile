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
	grep -rnI TODO --exclude-dir='_build/' | fgrep -v ignorethisparticularline

# Detect all appearances of the string YOLO and fail, unless it also has ignorethisparticularline2
.PHONY: yolos
yolos:
	! grep -rnI YOLO --exclude-dir='_build/' | fgrep -v ignorethisparticularline2

.PHONY: format
format:
	dune fmt

.PHONY: check-format
check-format:
	dune fmt --preview

.PHONY: clean
clean:
	dune clean
