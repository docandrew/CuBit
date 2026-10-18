#!/usr/bin/env bash
#  Hosted tests for the grant doorbell (CAP-002 step 3); --prove also runs
#  gnatprove (level 2) on Grant_Doorbells and Grant_Doorbell_ABI. Run in the
#  Nix shell. Proofs belong under systemd-run MemoryMax (see README).
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/grant_doorbells.gpr"
"$here/build/main" "${SEED:-20261017}"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/grant_doorbells.gpr" \
        -u grant_doorbells.adb \
        --level=2 -j2 --checks-as-errors=on
fi
