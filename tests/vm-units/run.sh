#!/usr/bin/env bash
#  VMM-001 slice 1: hosted tests of Guest_Tables, VM_Admission and
#  Guest_Speculation; --prove adds gnatprove level 2 on the three kernel
#  units. Run in the Nix shell, proofs under a memory cap:
#    systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
#      bash tests/vm-units/run.sh --prove
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/vm_units_tests.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/vm_units_tests.gpr" \
        -u guest_tables.ads -u vm_admission.adb -u guest_speculation.adb --level=2 -j2 --checks-as-errors=on \
        --report=fail 2>&1 | tee "$here/build/proof.log"
    if grep -qE "(medium|high|error):" "$here/build/proof.log"; then
        echo "vm-units: proof FAILED" >&2
        exit 1
    fi
    echo "vm-units: proof OK"
fi
