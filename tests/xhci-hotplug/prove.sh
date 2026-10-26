#!/usr/bin/env bash
# SPARK proofs of xhci.drv's ring, port, recovery, role, identity and
# mailbox units (level 2). From the repository root, in the Nix shell, under
# a memory limit (memory note: cap-proof-memory):
#   systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
#     nix develop -c bash tests/xhci-hotplug/prove.sh
set -euo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
cd "$root/kernel"
alr exec -- gnatprove -P ../tests/xhci-hotplug/xhci_hotplug_tests.gpr \
    -u xhci_registers.ads xhci_rings.adb xhci_port_machine.adb xhci_endpoint_recovery.adb \
       xhci_controller_identity.ads usb_device_roles.ads xhci_completions.adb xhci_ports.adb \
       driver_log_grant.adb \
    --level=2 --report=fail --checks-as-errors=on -j2
cat ../tests/xhci-hotplug/build/gnatprove/gnatprove.out | sed -n '/Summary of SPARK/,/^$/p'
