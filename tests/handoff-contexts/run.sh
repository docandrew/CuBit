#!/usr/bin/env bash
#  Which xHCI controllers pass DMA through until devmgr's firmware handoff
#  (kernel/src/handoff_contexts, DMA_Remapping). Hosted test;
#  --prove adds gnatprove level 2 (run under a memory cap):
#    systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
#      bash tests/handoff-contexts/run.sh --prove
#  Mutation check: bash tests/handoff-contexts/mutate.sh
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/handoff_contexts_tests.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/handoff_contexts_tests.gpr" \
        -u handoff_contexts.adb --level=2 -j2 --checks-as-errors=on \
        --report=fail 2>&1 | tee "$here/build/proof.log"
    if grep -qE "(medium|high|error):" "$here/build/proof.log"; then
        echo "handoff-contexts: proof FAILED" >&2
        exit 1
    fi
    echo "handoff-contexts: proof OK"
fi
