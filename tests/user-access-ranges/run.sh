#!/usr/bin/env bash
#  KERN-014 slice 2: user ranges the kernel accesses through the current
#  process's own mapping (kernel/src/user_access_ranges). Hosted test;
#  --prove adds gnatprove level 2 (run under a memory cap):
#    systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
#      bash tests/user-access-ranges/run.sh --prove
#  Mutation check: bash tests/user-access-ranges/mutate.sh
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/user_access_ranges_tests.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/user_access_ranges_tests.gpr" \
        -u user_access_ranges.adb --level=2 -j2 --checks-as-errors=on \
        --report=fail 2>&1 | tee "$here/build/proof.log"
    if grep -qE "(medium|high|error):" "$here/build/proof.log"; then
        echo "user-access-ranges: proof FAILED" >&2
        exit 1
    fi
    echo "user-access-ranges: proof OK"
fi
