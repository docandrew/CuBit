#!/usr/bin/env bash
#  Hosted tests of devmgr's claim ledgers on the heap and its firmware-data
#  decision (LIM-001b); --prove also runs gnatprove at level 2 on the proved
#  cores. Run in the Nix shell from anywhere.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/devmgr_claims_tests.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/devmgr_claims_tests.gpr" \
        -u devmgr_port_claims.adb -u devmgr_memory_claims.adb -u devmgr_data_windows.adb \
        -u cubit-firmware_data_exchange.ads \
        --level=2 -j2 --checks-as-errors=on
fi
