#!/usr/bin/env bash
#  Hosted tests for Capability_Install and Capability_Install.Placement
#  (CAP-002: what may be copied into another process's handle table, and
#  when); --prove also runs gnatprove at level 2. Run in the Nix shell.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/install.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/install.gpr" \
        -u capability_install.adb -u test_install.ads -u capability_abi.adb \
        --level=2 -j2 --checks-as-errors=on --report=all
    cat "$here/build/gnatprove/gnatprove.out"
fi
