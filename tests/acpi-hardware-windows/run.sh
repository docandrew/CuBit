#!/usr/bin/env bash
#  Hosted tests of the ACPI fixed-hardware windows (CuBit.ACPI_Fixed_Windows),
#  their startup names, devmgr's port claims (Devmgr_Port_Claims) and its
#  ACPI memory claims (Devmgr_Memory_Claims) and the memory windows' kernel-
#  tag classes (CuBit.ACPI_Memory_Classes); --prove
#  also runs gnatprove at level 2. Run in the Nix shell from anywhere.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/acpi_hardware_windows_tests.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/acpi_hardware_windows_tests.gpr" \
        -u cubit-acpi_fixed_windows.adb -u devmgr_port_claims.adb \
        -u devmgr_memory_claims.adb -u cubit-acpi_hardware_authority.ads \
        -u cubit-acpi_memory_classes.ads \
        --level=2 -j2 --checks-as-errors=on
fi
