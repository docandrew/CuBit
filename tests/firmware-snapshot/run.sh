#!/usr/bin/env bash
# FIRMWARE_SNAPSHOT (Firmware_Snapshot_ABI, kernel Firmware_Snapshot_Service):
# hosted checks of the wire ABI and the service body over a test snapshot;
# --prove adds the SPARK proof (level 2) of both, the service through a
# SPARK instance over a sealed snapshot (prove/proof_instance.ad?).
# Run inside the Nix shell. gnatprove's memory is capped: run it under
#   systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/firmware_snapshot_tests.gpr"
"$here/build/firmware_snapshot_tests"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/prove.gpr" -j2 --level=2 --checks-as-errors=on \
        -u firmware_snapshot_abi.adb -u proof_instance.adb --report=fail
    alr exec -- gnatprove -P "$here/prove_runtime.gpr" -j2 --level=2 --checks-as-errors=on \
        -u cubit-firmware_snapshot.adb --report=fail
fi
