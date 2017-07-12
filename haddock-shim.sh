#!/usr/bin/env bash

(
flock -s 200
$HADDOCK_EXE "$@" +RTS -N1 -tstats --machine-readable -RTS
cat << EOF >> $HADDOCK_SHIM_OUTPUT_FILE
===
$(pwd)
$(cat stats)
EOF
) 200> haddock-shim.lock
