#!/usr/bin/env bash

set -eu +x

ROOT=$(pwd)

IMAGE=measure-ghc

WORKDIR=$ROOT/work
EXTRA_BIN_DIR=$WORKDIR/extra-bin
STACK_WORK_DIR=$WORKDIR/inner.stack-work
CABAL_DIR=$WORKDIR/inner.cabal
STACK_DIR=$WORKDIR/inner.stack
MEASURE_GHC_SRC_DIR=$ROOT
BINDIST_DIR=$ROOT/bindists
FOUNDATION_STACK_WORK_DIR=$WORKDIR/inner.foundatation.stack-work
GENERICLENSLABELS_STACK_WORK_DIR=$WORKDIR/inner.generic-lens-labels.stack-work
STACKAGECURATOR_STACK_WORK_DIR=$WORKDIR/inner.stackage-curator.stack-work
SQLITEEXTENSIONFUNCTIONS_STACK_WORK_DIR=$WORKDIR/inner.sqlite-extension-functions.stack-work
mkdir -p \
  "$WORKDIR" \
  "$EXTRA_BIN_DIR" \
  "$STACK_WORK_DIR" \
  "$CABAL_DIR" \
  "$STACK_DIR" \
  "$MEASURE_GHC_SRC_DIR/.stack-work" \
  "$MEASURE_GHC_SRC_DIR/foundation/.stack-work" \
  "$MEASURE_GHC_SRC_DIR/stackage-curator/.stack-work" \
  "$MEASURE_GHC_SRC_DIR/generic-lens-labels/.stack-work" \
  "$MEASURE_GHC_SRC_DIR/sqlite-extension-functions/.stack-work" \
  "$FOUNDATION_STACK_WORK_DIR" \
  "$GENERICLENSLABELS_STACK_WORK_DIR" \
  "$STACKAGECURATOR_STACK_WORK_DIR" \
  "$SQLITEEXTENSIONFUNCTIONS_STACK_WORK_DIR"

ARGS_COMMON="--rm -v $BINDIST_DIR:$HOME/bindists:ro -v $WORKDIR:$HOME/work -w $HOME -v $MEASURE_GHC_SRC_DIR:$HOME/measure-ghc -v /etc/passwd:/etc/passwd:ro -v /etc/group:/etc/group:ro -v /etc/sudoers:/etc/sudoers:ro -v $CABAL_DIR:$HOME/.cabal:rw -v $STACK_DIR:$HOME/.stack:rw -v $STACK_WORK_DIR:$HOME/measure-ghc/.stack-work:rw -v $SQLITEEXTENSIONFUNCTIONS_STACK_WORK_DIR:$HOME/measure-ghc/sqlite-extension-functions/.stack-work:rw -v $STACKAGECURATOR_STACK_WORK_DIR:$HOME/measure-ghc/stackage-curator/.stack-work:rw -v $FOUNDATION_STACK_WORK_DIR:$HOME/measure-ghc/foundation/.stack-work:rw -v $GENERICLENSLABELS_STACK_WORK_DIR:$HOME/measure-ghc/generic-lens-labels/.stack-work:rw"

MEASURE_GHC_BINDIST="ghc-8.3.20170813-x86_64-unknown-linux.tar.xz"
# Do the rest of the pre-build actions:
#
# * Check that the plan is valid
# * Fetch all needed tarballs (the build step does not have write access to the tarball directory)
# * Do a single unpack to create the package index cache (again due to directory perms)

docker run $ARGS_COMMON $IMAGE /bin/bash -c "chown $USER $HOME"

docker run $ARGS_COMMON $IMAGE /bin/bash -c "exec sudo -E -u $USER env \"HOME=$HOME\" \"PATH=\$PATH\" bash -c \"cd measure-ghc && time stack setup --ghc-bindist $HOME/bindists/$MEASURE_GHC_BINDIST && time stack build\""

# Now do the actual build. We need to first set the owner of the home directory
# correctly, so we run the command as root, change owner, and then use sudo to
# switch back to the current user
docker run $ARGS_COMMON $IMAGE /bin/bash -c "exec sudo -E -u $USER env \"HOME=$HOME\" \"PATH=\$PATH\" bash -c \"time stack --stack-yaml measure-ghc/stack.yaml exec experiment\""

echo -n "Completed at "
date
