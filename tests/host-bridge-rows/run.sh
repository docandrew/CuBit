#!/usr/bin/env bash
#  Hosted tests of devmgr's host-bridge window rows (PCI-BAR-001); --prove
#  also runs gnatprove at level 2. Run in the Nix shell from anywhere.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/host_bridge_rows_tests.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/host_bridge_rows_tests.gpr" \
        -u host_bridge_rows.adb --level=2 -j2 --checks-as-errors=on
fi
