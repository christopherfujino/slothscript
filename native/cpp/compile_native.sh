#!/usr/bin/env bash

set -euo pipefail

# Don't use sloth for this, in case native changes break the build
# Intended to be called by dune

# configure
cmake .

# build
cmake --build .
