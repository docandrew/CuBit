#!/usr/bin/env bash
#  Hosted tests for CuBit.Program_Descriptions; --prove also runs gnatprove.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/program_descriptions.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/program_descriptions.gpr" -u cubit-program_descriptions.adb \
        --level=2 -j4 --checks-as-errors=on
fi
