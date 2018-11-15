#!/bin/bash

echo -en "travis_fold:start:install.python\\r"

set -uex

sudo apt update -qq
sudo apt install -y python2.7 python-pip

python --version
pip --version

echo -en "travis_fold:end:install.python\\r"
