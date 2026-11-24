#!/usr/bin/env bash
#  The TSC agreement rules (kernel/src/tsc_sync_rules.ads, TSC_Sync): each
#  CPU's IA32_TSC_ADJUST, the warp check's verdict, compensation, and whether
#  the clock stays shared. Hosted test; --prove adds gnatprove level 2 (run
#  under a memory cap):
#    systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
#      bash tests/tsc-sync/run.sh --prove
#  Mutation check: bash tests/tsc-sync/mutate.sh
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/tsc_sync_tests.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/tsc_sync_tests.gpr" \
        -u tsc_sync_rules.ads --level=2 -j2 --checks-as-errors=on \
        --report=fail 2>&1 | tee "$here/build/proof.log"
    if grep -qE "(medium|high|error):" "$here/build/proof.log"; then
        echo "tsc-sync: proof FAILED" >&2
        exit 1
    fi
    echo "tsc-sync: proof OK"
fi
