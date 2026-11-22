#!/usr/bin/env bash
#  KERN-014 slice 4: one process-pool region (free and clean frames,
#  aligned runs; kernel/src/pool_region_maps). Hosted test;
#  --prove adds gnatprove level 2 (run under a memory cap):
#    systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
#      bash tests/pool-region-maps/run.sh --prove
#  Mutation check: bash tests/pool-region-maps/mutate.sh
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/pool_region_maps_tests.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/pool_region_maps_tests.gpr" \
        -u pool_region_maps.adb --level=2 -j2 --checks-as-errors=on \
        --report=fail 2>&1 | tee "$here/build/proof.log"
    if grep -qE "(medium|high|error):" "$here/build/proof.log"; then
        echo "pool-region-maps: proof FAILED" >&2
        exit 1
    fi
    echo "pool-region-maps: proof OK"
fi
