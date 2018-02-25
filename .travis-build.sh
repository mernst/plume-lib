#!/bin/bash
ROOT=$TRAVIS_BUILD_DIR/..

# Fail the whole script if any command fails
set -e

if [[ "${TYPECHECK}" != "true" ]]; then

  make USE_CODECOV=1 all-but-emacs check

else

## Build Checker Framework
(cd $ROOT && git clone https://github.com/typetools/checker-framework.git) || (cd $ROOT && git clone https://github.com/typetools/checker-framework.git)
# This also builds annotation-tools and jsr308-langtools
(cd $ROOT/checker-framework/ && ./.travis-build-without-test.sh downloadjdk)
export CHECKERFRAMEWORK=$ROOT/checker-framework

## No need to do this -- plume-lib already exists.
# ## Obtain plume-lib
# (cd $ROOT && git clone https://github.com/mernst/plume-lib.git) || (cd $ROOT && git clone https://github.com/mernst/plume-lib.git)

# -Afilenames is to prevent the job from timing out if it goes 10 minutes without output
make -C $ROOT/plume-lib/java JAVACHECK_EXTRA_ARGS=-Afilenames check-types

fi
