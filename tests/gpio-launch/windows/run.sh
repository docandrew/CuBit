#!/usr/bin/env bash
#  Hosted tests of Devmgr_ACPI_Windows (docs/device-matching.md "ACPI
#  launch"); --prove also runs gnatprove at level 2. Run in the Nix shell;
#  proofs under a memory cap:
#    systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
#      bash tests/gpio-launch/windows/run.sh --prove
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../../kernel"
alr exec -- gprbuild -p -q -P "$here/acpi_windows_tests.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/acpi_windows_tests.gpr" -u devmgr_acpi_windows.adb -u devmgr_phase_declarations.adb \
        -u devmgr_hid_offers.adb \
        --level=2 -j2 --checks-as-errors=on
fi
