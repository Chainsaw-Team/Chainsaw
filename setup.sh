#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o xtrace

src/main/resources/install/installSimulators.sh
src/main/resources/install/installPython.sh
src/main/resources/install/installFlopoco.sh