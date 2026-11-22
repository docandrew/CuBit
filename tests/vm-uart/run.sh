#!/usr/bin/env bash
#  VMM-001: hosted tests of vmm's Guest_UART (the 16550A receive path and
#  its IRQ edge), Serial_Keys and Console_Line; --prove adds gnatprove
#  level 2 on all three (no unproved check, nothing justified). Run in the
#  Nix shell, proofs under a memory cap:
#    systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
#      bash tests/vm-uart/run.sh --prove
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/vm_uart_tests.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/vm_uart_tests.gpr" \
        -u guest_uart.adb -u serial_keys.adb -u console_line.adb --level=2 -j2 --checks-as-errors=on \
        --report=fail 2>&1 | tee "$here/build/proof.log"
    if grep -qE "(medium|high|error|warning):" "$here/build/proof.log" ||
       grep -rqs "pragma Annotate\|Justif" "$here/../../userspace/apps/vmm/core/guest_uart.adb" \
         "$here/../../userspace/apps/vmm/core/console_line.adb" "$here/../../userspace/apps/vmm/serial_keys.adb"; then
        echo "vm-uart: proof FAILED" >&2
        exit 1
    fi
    echo "vm-uart: proof OK"
fi
