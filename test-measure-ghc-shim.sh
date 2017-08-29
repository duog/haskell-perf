#!/usr/bin/env bash

pushd transformers-compat-0.5.1.4
unset GHC_PACKAGE_PATH

cabal sandbox delete
cabal sandbox init

cat > measure-ghc-shim.yaml << EOF
_ss_executableFile: ghc
_ss_statsFile: $(pwd)/stats
_ss_statsFileLock: $(pwd)/stats.lock
_ss_enabled : true
EOF

export MEASURE_SHIM_FILE_ghc=$(pwd)/measure-ghc-shim.yaml

cabal configure --with-ghc $(which ghc-shim) -v
cabal build

popd
