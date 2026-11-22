#!/usr/bin/env bash
#  Hosted tests for CuBit.Launch_Grants; --prove also runs gnatprove.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/launch_grants.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/launch_grants.gpr" -u cubit-launch_grants.adb \
        --level=2 -j4 --checks-as-errors=on
fi
