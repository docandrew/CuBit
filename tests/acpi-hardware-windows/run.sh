#!/usr/bin/env bash
#  Hosted tests of the ACPI fixed-hardware windows (CuBit.ACPI_Fixed_Windows),
#  their startup names and devmgr's port claims (Devmgr_Port_Claims); --prove
#  also runs gnatprove at level 2. Run in the Nix shell from anywhere.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/acpi_hardware_windows_tests.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/acpi_hardware_windows_tests.gpr" \
        -u cubit-acpi_fixed_windows.adb -u devmgr_port_claims.adb \
        --level=2 -j2 --checks-as-errors=on
fi
