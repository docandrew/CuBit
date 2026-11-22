#!/usr/bin/env bash
#  VMM-001 slice 0: hosted tests of Virtualization_Features (the boot
#  report's decoder); --prove adds gnatprove level 2 on the kernel unit.
#  Run in the Nix shell, proofs under a memory cap:
#    systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
#      bash tests/virtualization-features/run.sh --prove
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/virtualization_features_tests.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/virtualization_features_tests.gpr" \
        -u virtualization_features.adb --level=2 -j2 --checks-as-errors=on \
        --report=fail 2>&1 | tee "$here/build/proof.log"
    if grep -qE "(medium|high|error):" "$here/build/proof.log"; then
        echo "virtualization-features: proof FAILED" >&2
        exit 1
    fi
    echo "virtualization-features: proof OK"
fi
