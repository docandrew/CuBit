#!/usr/bin/env bash
#  Hosted tests for DOOM's Ada platform layer (userspace/ports/doom,
#  docs/c-removal.md), the layout check against doomgeneric's headers, and
#  with --prove gnatprove. Run in the Nix shell from anywhere.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/doom_port.gpr"
"$here/build/main"
python3 "$here/check_layout.py"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/doom_port.gpr" -u cubit-doom_keys.adb \
        -u cubit-doom_lumps.adb -u cubit-doom_mixer.adb \
        --level=2 -j4 --checks-as-errors=on
fi
