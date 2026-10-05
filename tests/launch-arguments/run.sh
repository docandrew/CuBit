#!/usr/bin/env bash
#  Hosted tests for CuBit.Launch_Arguments; --prove also runs gnatprove.
#  Run in the Nix shell from anywhere.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/launch_arguments.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/launch_arguments.gpr" \
        -u cubit-launch_arguments.adb -u cubit-child_exits.ads \
        -u process_launch.ads -u cubit-launch_authority.adb --level=2 -j4 --checks-as-errors=on
fi
