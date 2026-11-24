#!/usr/bin/env bash
#  Who keeps the system's time on a timer interrupt, and what msTicks
#  becomes (kernel/src/timekeeping_turns.ads, Time.clockTick). Hosted test;
#  --prove adds gnatprove level 2 (run under a memory cap):
#    systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
#      bash tests/timekeeping-turns/run.sh --prove
#  Mutation check: bash tests/timekeeping-turns/mutate.sh
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/timekeeping_turns_tests.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/timekeeping_turns_tests.gpr" \
        -u timekeeping_turns.ads --level=2 -j2 --checks-as-errors=on \
        --report=fail 2>&1 | tee "$here/build/proof.log"
    if grep -qE "(medium|high|error):" "$here/build/proof.log"; then
        echo "timekeeping-turns: proof FAILED" >&2
        exit 1
    fi
    echo "timekeeping-turns: proof OK"
fi
