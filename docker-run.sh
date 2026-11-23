#!/usr/bin/env bash

IMAGE_NAME=sloth-ci

exec docker run \
  --tty --interactive \
  --user root \
  --rm \
  "$IMAGE_NAME" \
  /bin/bash -i
