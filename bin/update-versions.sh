#!/bin/bash -eux
find $(git rev-parse --show-toplevel) -maxdepth 3 -name 'ambiata-mismi-*.cabal' \
  | xargs -n1 \
    sed -i -E "s;^(version: +).*;\1${1};"
