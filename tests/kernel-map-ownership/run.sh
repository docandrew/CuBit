#!/usr/bin/env bash
#  KERN-014 ownership model (docs/kernel-memory-map.md): hosted test, and
#  with --prove gnatprove at level 2 (run under a memory cap):
#    systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
#      bash tests/kernel-map-ownership/run.sh --prove
#  Mutation check (the proof must fail for each weakened precondition):
#    bash tests/kernel-map-ownership/mutate.sh
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/kernel_map_ownership.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/kernel_map_ownership.gpr" \
        -u kernel_map_ownership.adb --level=2 -j2 --checks-as-errors=on \
        --report=fail 2>&1 | tee "$here/build/proof.log"
    if grep -qE "(medium|high|error):" "$here/build/proof.log"; then
        echo "kernel-map-ownership: proof FAILED" >&2
        exit 1
    fi
    echo "kernel-map-ownership: proof OK"
fi
