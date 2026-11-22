#!/usr/bin/env bash
#  DBG-001: debug-session admission (kernel/src/debug_admission) and the
#  debug-view page walk (kernel/src/debug_view_walk). Hosted test; --prove
#  adds gnatprove level 2 (run under a memory cap):
#    systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
#      bash tests/debug-sessions/run.sh --prove
#  Mutation check: bash tests/debug-sessions/mutate.sh
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/debug_sessions_tests.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/debug_sessions_tests.gpr" \
        -u debug_admission.adb -u debug_view_walk.adb -u walk_proof.adb -u debug_abi.ads \
        --level=2 -j2 --checks-as-errors=on \
        --report=fail 2>&1 | tee "$here/build/proof.log"
    if grep -qE "(medium|high|error):" "$here/build/proof.log"; then
        echo "debug-sessions: proof FAILED" >&2
        exit 1
    fi
    echo "debug-sessions: proof OK"
fi
