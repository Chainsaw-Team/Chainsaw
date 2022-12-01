#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o xtrace

PYTHON_BIN=`which python3`
export PYTHON=$PYTHON_BIN

# auto test for Chainsaw pr
sbt test

# sbt commands for regression test
# sbt testOnly Chainsaw.* -- -l *Isca2023Test *

