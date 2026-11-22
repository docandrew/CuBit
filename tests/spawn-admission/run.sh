#!/usr/bin/env bash
#  Hosted tests of the kernel's SPAWN admission rules (CAP-012:
#  kernel/src/spawn_admission.ads); --prove also runs gnatprove at level 2
#  on the unit. Run in the Nix shell; the caller bounds gnatprove's memory.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/spawn_admission_tests.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/spawn_admission_tests.gpr" \
        -u spawn_admission.adb --level=2 -j2 --checks-as-errors=on --report=all
    grep -E "^(Summary|Total|Run-time|Assertions|Functional)" -A0 \
        "$here/build/gnatprove/gnatprove.out" || true
fi
