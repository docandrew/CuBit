#!/usr/bin/env bash
#  Hosted tests of service switches and system generation records;
#  --prove also runs gnatprove (level 2, -j2, under a 24 GiB memory cap).
#  Run in the Nix shell from anywhere. The QEMU acceptance test is
#  tests/headless/run.sh --test service-switch --accel kvm.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/service_switch.gpr"
"$here/build/switch_tests"
alr exec -- gprbuild -p -q -P "$here/host-records/generation_records.gpr"
"$here/host-records/build/generation_records_tests"
if [[ ${1:-} == --prove ]]; then
    systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
        alr exec -- gnatprove -P "$here/service_switch.gpr" \
        -u cubit-service_replacement.adb -u service_switches.adb -u manifest_fingerprints.adb \
        --level=2 -j2 --checks-as-errors=on
fi
