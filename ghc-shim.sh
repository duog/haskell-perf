#!/usr/bin/env bash

(
flock -s 200
$GHC_EXE "$@" +RTS -N1 -tstats --machine-readable -RTS
cat << EOF >> $GHC_SHIM_OUTPUT_FILE
===
$(pwd)
$(cat stats)
EOF
) 200> ghc-shim.lock