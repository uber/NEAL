#!/bin/bash

set -e

brew install opam
opam init -n
opam switch 4.04.0
eval "$(opam config env)"
make setup
make coverage
