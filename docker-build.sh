#!/usr/bin/env bash

set -euxo pipefail

IMAGE_NAME=sloth-ci
DIR=$(dirname $(realpath "${BASH_SOURCE[0]}") )

docker build --tag "$IMAGE_NAME" "$DIR"

docker image prune --force
