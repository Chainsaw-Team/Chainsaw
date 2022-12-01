#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o xtrace

sudo apt-get update
sudo apt-get install -y python3-pip
pip install numpy
pip install scipy
pip install matplotlib

# for human user only
# jupyter notebook
# pip install notebook
# pip install jinja2  --upgrade

PYTHON_BIN=`which python3`
echo  | cat >> ~/.bashrc # empty line
echo export PYTHON=$PYTHON_BIN | cat >> ~/.bashrc
echo alias python=$PYTHON_BIN | cat >> ~/.bashrc