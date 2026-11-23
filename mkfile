# TODO: what should our default target be?
MKSHELL = bash
DB_REPO = `{realpath "$PWD/../doc-builder"}
DB_BINARY = "$DB_REPO/db"

unit-tests:V:
  OUNIT_CI=true dune build @runtest --force

dune-build:V:
  dune build

native-host/slothnative.exe:
  pushd native-host >/dev/null
    mk
  popd >/dev/null

ci:V:
  echo "DB_REPO is $DB_REPO"
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

$DB_BINARY:
  echo "$DB_REPO"
  pushd "$DB_REPO" >/dev/null
    cat mkfile
    mk build
  popd >/dev/null

docs:V: $DB_BINARY
  echo "$DB_REPO"
  ls -alh "$DB_REPO"
  echo "$DB_BINARY"
  ls -alh "$DB_BINARY"
  "$DB_BINARY" -ensure

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
