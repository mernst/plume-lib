#!/bin/bash
ROOT=$TRAVIS_BUILD_DIR/..

# Fail the whole script if any command fails
set -e

## Build Checker Framework
# (cd $ROOT && git clone https://github.com/typetools/checker-framework.git)
## Temporary: use a fork of the Checker Framework
(cd $ROOT && git clone https://github.com/panacekcz/checker-framework.git --branch strings-indexof)
# This also builds annotation-tools and jsr308-langtools
(cd $ROOT/checker-framework/ && ./.travis-build-without-test.sh downloadjdk)
export CHECKERFRAMEWORK=$ROOT/checker-framework

make -C java error-prone
make -C java check-types
