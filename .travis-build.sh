#!/bin/bash
ROOT=$TRAVIS_BUILD_DIR/..

# Fail the whole script if any command fails
set -e

## Build Checker Framework
(cd $ROOT && git clone https://github.com/typetools/checker-framework.git)
# This also builds annotation-tools and jsr308-langtools
(cd $ROOT/checker-framework/ && ./.travis-build-without-test.sh downloadjdk)
export CHECKERFRAMEWORK=$ROOT/checker-framework

## Obtain plume-lib
(cd $ROOT && git clone https://github.com/mernst/plume-lib.git)

make -C $ROOT/plume-lib/java check-types
