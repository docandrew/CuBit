#!/usr/bin/env bash
#  Hosted tests for CuBit.Path_Names; --prove also runs gnatprove.
#  Run in the Nix shell from anywhere.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/path_names.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/path_names.gpr" -u cubit-path_names.adb \
        --level=2 -j4 --checks-as-errors=on
fi
