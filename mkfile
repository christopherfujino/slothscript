# TODO: what should our default target be?
MKSHELL = bash

unit-tests:V:
  OUNIT_CI=true dune build @runtest --force

dune-build:V:
  dune build

native-host/slothnative.exe:
  pushd native-host >/dev/null
    mk
  popd >/dev/null

ci:V:
  # ensure these run sequentially
  mk docs
  mk fetch-deps
  mk unit-tests
  pushd native-host >/dev/null
    # This will run integration tests
    mk ci
  popd >/dev/null
  mk check-format

train:V:
  TEST_DIR="${PWD}/test" dune build @train

docs:V:
  db -ensure

fetch-deps:V:
  opam install "$PWD" --yes --deps-only --with-test --with-doc -vv

check-format:V:
  dune fmt --preview

todos:V:
  # TODO: create expectations for this
  grep -rnI TODO --exclude-dir='_build/' | fgrep -v ignorethisparticularline

clean:V:
  dune clean
  pushd native-host >/dev/null
    mk clean
  popd >/dev/null
