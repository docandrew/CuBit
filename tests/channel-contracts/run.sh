#!/usr/bin/env bash
#  Hosted tests for CuBit.Channel_Contracts (docs/data-plane.md); --prove
#  also runs gnatprove (level 1). Run in the Nix shell.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/contracts.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/contracts.gpr" -u cubit-channel_contracts.adb \
        -u cubit-channel_protocol.ads \
        --level=1 -j2 --checks-as-errors=on
fi
