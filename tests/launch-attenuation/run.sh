#!/usr/bin/env bash
#  Hosted tests of procmgr's launch attenuation rule (CAP-012:
#  userspace/services/procmgr/launch_attenuation.ads); --prove also runs
#  gnatprove at level 2. Run in the Nix shell; bound gnatprove's memory.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/launch_attenuation_tests.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/launch_attenuation_tests.gpr" \
        -u launch_attenuation.adb -u session_launch.adb --level=2 -j2 --checks-as-errors=on --report=all
    grep -E "^(Summary|Total|Run-time|Assertions|Functional)" \
        "$here/build/gnatprove/gnatprove.out" || true
fi
