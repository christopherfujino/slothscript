#!/usr/bin/env bash

set -euo pipefail

SCRIPTDIR="$( dirname "$( realpath "${BASH_SOURCE[0]}" )" )"

BUILD="${SCRIPTDIR}/build"

cmake --build "$BUILD"
"${BUILD}/test"
