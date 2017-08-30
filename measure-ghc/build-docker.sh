#!/usr/bin/env bash

THIS_DIR=$(cd $(dirname $0); pwd)
# Get latest stack
# curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C $THIS_DIR '*/stack'

pushd $THIS_DIR
docker build --label duog/measure-ghc -t measure-ghc .
popd
