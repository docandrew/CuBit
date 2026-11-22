#!/usr/bin/env bash
#  KERN-014 slice 1: the buddy allocator's out-of-band free-list link codes
#  (kernel/src/free_link_codes). Hosted test; --prove adds gnatprove level 2
#  (run under a memory cap):
#    systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
#      bash tests/free-link-codes/run.sh --prove
#  Mutation check: bash tests/free-link-codes/mutate.sh
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/free_link_codes_tests.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/free_link_codes_tests.gpr" \
        -u free_link_codes.adb --level=2 -j2 --checks-as-errors=on \
        --report=fail 2>&1 | tee "$here/build/proof.log"
    if grep -qE "(medium|high|error):" "$here/build/proof.log"; then
        echo "free-link-codes: proof FAILED" >&2
        exit 1
    fi
    echo "free-link-codes: proof OK"
fi
