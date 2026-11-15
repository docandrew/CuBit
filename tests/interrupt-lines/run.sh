#!/usr/bin/env bash
#  Hosted tests of legacy interrupt lines (Interrupt_Line_ABI, the kernel's
#  Interrupt_Line_Policy, CuBit.ACPI_SCI_Route, acpi.svc's ACPI_SCI_Registers);
#  --prove also runs gnatprove
#  at level 2 on them. Run in the Nix shell from anywhere. Proofs: under
#  systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/interrupt_lines.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/interrupt_lines.gpr" \
        -u interrupt_line_abi.adb -u interrupt_line_policy.adb \
        -u cubit-acpi_sci_route.adb -u acpi_sci_registers.adb -u acpi_sci_quiesce.adb \
        --level=2 -j2 --checks-as-errors=on
fi
