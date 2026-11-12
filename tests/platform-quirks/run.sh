#!/usr/bin/env bash
#  Hosted tests of devmgr's platform quirks (ACPI-POOL-001): the shared
#  Platform_Identity and quirk-table pattern, the quirk table and the P2SB
#  step classification, with the IdeaPad's own facts. --prove also runs gnatprove on
#  the proved units (level 1, level 2 for what needs it). Run in the Nix
#  shell, proofs under a memory cap:
#    systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
#      bash tests/platform-quirks/run.sh --prove
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/platform_quirks_tests.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    mkdir -p "$here/build"
    alr exec -- gnatprove -P "$here/platform_quirks_tests.gpr" \
        -u platform_identity.adb -u platform_quirks.adb \
        -u platform_quirks-intel_p2sb.adb -u platform_quirks-intel_gpio_communities.adb \
        --level=${PROOF_LEVEL:-1} -j2 --checks-as-errors=on --report=fail \
        2>&1 | tee "$here/build/proof.log"
    if grep -qE "(medium|high|error):" "$here/build/proof.log"; then
        echo "platform-quirks: proof FAILED" >&2
        exit 1
    fi
    echo "platform-quirks: proof OK"
fi
