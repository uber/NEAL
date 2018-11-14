#!/bin/bash

echo -en "travis_fold:start:install.opam\\r"

# Allows for specifying a specific branch or tag to use from the ocaml ci scripts repo
ocaml_ci_version=${OCAML_CI_VERSION:-master}

### Bootstrap

set -uex

get() {
  wget "https://raw.githubusercontent.com/ocaml/ocaml-ci-scripts/${ocaml_ci_version}/$1"
}

get .travis-ocaml.sh
sh .travis-ocaml.sh

eval $(opam config env)

echo -en "travis_fold:end:install.opam\\r"
