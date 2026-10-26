#!/usr/bin/env bash
# Hosted xhci hotplug/ring/recovery tests. From the repository root, in the
# Nix shell: bash tests/xhci-hotplug/run.sh
set -euo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
cd "$root/kernel"
alr exec -- gprbuild -q -p -P ../tests/xhci-hotplug/xhci_hotplug_tests.gpr
for test in ring_tests ehb_tests port_machine_tests recovery_tests; do
    "../tests/xhci-hotplug/build/$test"
done
echo "xhci-hotplug hosted tests: PASS"
