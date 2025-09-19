.PHONY: test
test:
	# OUNIT_CI=true makes output prettier
	# --force makes all tests run
	OUNIT_CI=true dune build @runtest --force
	#OUNIT_CI=true dune build @runtest

.PHONY: train
train:
	TEST_DIR=$(PWD)/test dune build @train

.PHONY: describe
describe:
	dune describe

.PHONY: repl
repl:
	dune exec repl

.PHONY: utop
utop: build
	# opam install utop
	dune utop lib/

.PHONY: build
build:
	dune build --verbose

.PHONY: get
get:
	opam install . --deps-only --with-test --with-doc -vv

.PHONY: list-errors
list-errors:
	dune exec -- menhir --unused-token COMMENT --list-errors lib/compiler/parser.mly | less

.PHONY: explain
explain: clean
	dune exec -- menhir --explain lib/compiler/parser.mly

.PHONY: ci
ci:
	dune exec tool/ci.exe

.PHONY: todos
todos:
	grep -rnI TODO --exclude-dir='_build/' | fgrep -v ignorethisparticularline

# Detect the string YOLO and fail, unless it also has ignorethisparticularline2
.PHONY: yolos
yolos:
	! grep -rnI YOLO --exclude-dir='_build/' | fgrep -v ignorethisparticularline2

.PHONY: format
format:
	dune fmt

.PHONY: check-format
check-format:
	dune fmt --preview

.PHONY: docs
docs:
	db -ensure

.PHONY: clean
clean:
	dune clean
