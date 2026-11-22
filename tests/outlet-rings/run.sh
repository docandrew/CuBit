#!/usr/bin/env bash
#  Hosted tests for CuBit.Outlet_Rings; --prove also runs gnatprove.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/outlet_rings.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/outlet_rings.gpr" -u cubit-outlet_rings.adb \
        --level=2 -j4 --checks-as-errors=on
fi
