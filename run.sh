#!/usr/bin/env bash

export GHC_TOP_DIR="$(pwd)/../8.2"

export HADDOCK_EXE=/opt/ghc/8.0.2/bin/haddock
export GHC_EXE=/opt/ghc/8.0.2/bin/ghc
export GHC_PKG_EXE=/opt/ghc/8.0.2/bin/ghc-pkg

mkdir results

export HADDOCK_SHIM_OUTPUT_FILE=`pwd`/results/haddock-8.0.2-$(date +"%Y%m%dT%H%M").out
export GHC_SHIM_OUTPUT_FILE=`pwd`/results/ghc-8.0.2.$(date +"%Y%m%dT%H%M").out

rm $HADDOCK_SHIM_OUTPUT_FILE
rm $GHC_SHIM_OUTPUT_FILE
cabal sandbox delete

cabal sandbox init
cabal install "$@" -j4 --enable-documentation --keep-going --with-ghc=`pwd`/ghc-shim.sh --with-ghc-pkg=${GHC_PKG_EXE} --with-haddock=`pwd`/haddock-shim.sh
echo "loading ghc stats"
stack exec -- measure-ghc -d d load -g -l 8.0.2 -i "$GHC_SHIM_OUTPUT_FILE"
echo "loading haddock stats: $HADDOCK_SHIM_OUTPUT_FILE"
stack exec -- measure-ghc -d d load -a -l 8.0.2 -i "'$HADDOCK_SHIM_OUTPUT_FILE'"
cabal sandbox delete

export HADDOCK_EXE=${GHC_TOP_DIR}/inplace/bin/haddock
export GHC_EXE=${GHC_TOP_DIR}/inplace/bin/ghc-stage2
export GHC_PKG_EXE=${GHC_TOP_DIR}/inplace/bin/ghc-pkg


for commit in f6f0fd9 7bb8472 42638f7; do
  export HADDOCK_SHIM_OUTPUT_FILE=`pwd`/results/haddock-${commit}-$(date +"%Y%m%dT%H%M").out
  export GHC_SHIM_OUTPUT_FILE=`pwd`/results/ghc-8.2.0-$(date +"%Y%m%dT%H%M").out
  rm $HADDOCK_SHIM_OUTPUT_FILE
  rm $GHC_SHIM_OUTPUT_FILE
  pushd $GHC_TOP_DIR
  pushd utils/haddock
  git checkout $commit
  popd
  make -j
  echo "$(pwd)/compiler" >> $HADDOCK_SHIM_OUTPUT_FILE
  cat compiler/stage2/haddock.t >> $HADDOCK_SHIM_OUTPUT_FILE
  echo "===" >> $HADDOCK_SHIM_OUTPUT_FILE
  echo "$(pwd)/libraries/base" >> $HADDOCK_SHIM_OUTPUT_FILE
  cat libraries/base/dist-install/haddock.t >> $HADDOCK_SHIM_OUTPUT_FILE
  echo "===" >> $HADDOCK_SHIM_OUTPUT_FILE
  echo "$(pwd)/libraries/Cabal" >> $HADDOCK_SHIM_OUTPUT_FILE
  cat libraries/Cabal/Cabal/dist-install/haddock.t >> $HADDOCK_SHIM_OUTPUT_FILE
  popd
  cabal sandbox init
  cabal install  "$@" -j4 --enable-documentation --keep-going --with-ghc=`pwd`/ghc-shim.sh --with-ghc-pkg=${GHC_PKG_EXE} --with-haddock=`pwd`/haddock-shim.sh --constraint="hashable == 1.2.6.1" --allow-newer=base
  stack exec -- measure-ghc -d d load -g -l 8.2.0 -i $GHC_SHIM_OUTPUT_FILE
  stack exec -- measure-ghc -d d load -a -l ${commit} -i $HADDOCK_SHIM_OUTPUT_FILE
  cabal sandbox delete
done
