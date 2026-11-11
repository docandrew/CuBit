#!/usr/bin/env bash
#  Hosted tests for ACPI-POOL-001 (the pool's range set, claims, mapping
#  tables and admission, sized from discovery; devmgr's discovery and its
#  sideband layout); --prove also runs gnatprove level 2 on all three. Run in the Nix shell, proofs under a memory cap:
#    systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
#      bash tests/platform-pool/run.sh --prove
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/pool.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/pool.gpr" \
        -u platform_pool_sets.adb -u devmgr_platform_pool.adb -u devmgr_sideband_layout.adb \
        --level=2 -j2 --checks-as-errors=on --report=fail \
        2>&1 | tee "$here/build/proof.log"
    if grep -qE "(medium|high|error):" "$here/build/proof.log"; then
        echo "platform-pool: proof FAILED" >&2
        exit 1
    fi
    echo "platform-pool: proof OK"
fi
