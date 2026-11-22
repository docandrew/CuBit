#!/usr/bin/env bash
#  Hosted tests for devmgr's PCI capability walk (PCI_Capabilities):
#  well-formed, truncated and looping lists, every reserved-bit and
#  boundary case, and random configuration spaces. --prove also runs
#  gnatprove level 2 on the walk (and its instance used by the tests).
#  Run in the Nix shell; prove under a memory cap (coordination notes).
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/pci_capabilities_tests.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/pci_capabilities_tests.gpr" \
        -u pci_capabilities.adb -u config_spaces.ads \
        --level=2 -j2 --checks-as-errors=on --report=fail
fi
