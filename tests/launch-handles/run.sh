#!/usr/bin/env bash
#  Hosted tests of the launch handle's decisions (CAP-012, DBG-001:
#  userspace/services/procmgr/handle_admission.ads, CuBit.Launch_Handles);
#  --prove adds gnatprove at level 2. Nix shell, memory-capped:
#    systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
#      bash tests/launch-handles/run.sh --prove
#  Mutation check: bash tests/launch-handles/mutate.sh
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/launch_handles_tests.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/launch_handles_tests.gpr" \
        -u handle_admission.adb -u cubit-launch_handles.ads --level=2 -j2 \
        --checks-as-errors=on --report=fail 2>&1 | tee "$here/build/proof.log"
    if grep -qE "(medium|high|error):" "$here/build/proof.log"; then
        echo "launch-handles: proof FAILED" >&2
        exit 1
    fi
    grep -E "^(Summary|Total|Run-time|Assertions|Functional)" \
        "$here/build/obj/gnatprove/gnatprove.out" || true
    echo "launch-handles: proof OK"
fi
