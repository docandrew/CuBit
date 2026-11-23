#!/usr/bin/env bash
#  VMM-001: VMX control-register access exits (kernel/src/vmx_cr_access).
#  Hosted test; --prove adds gnatprove level 2 (run under a memory cap):
#    systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
#      bash tests/vmx-cr-access/run.sh --prove
#  Mutation check: bash tests/vmx-cr-access/mutate.sh
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/vmx_cr_access_tests.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/vmx_cr_access_tests.gpr" \
        -u vmx_cr_access.adb --level=2 -j2 --checks-as-errors=on \
        --report=fail 2>&1 | tee "$here/build/proof.log"
    if grep -qE "(medium|high|error):" "$here/build/proof.log"; then
        echo "vmx-cr-access: proof FAILED" >&2
        exit 1
    fi
    echo "vmx-cr-access: proof OK"
fi
