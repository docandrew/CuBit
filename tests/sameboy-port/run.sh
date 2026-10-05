#!/usr/bin/env bash
#  Hosted tests for SameBoy's Ada frontend (userspace/ports/sameboy,
#  docs/c-removal.md), the check against the SameBoy core's headers, and
#  with --prove gnatprove. Run in the Nix shell from anywhere.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/sameboy_port.gpr"
"$here/build/main"
python3 "$here/check_core.py"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/sameboy_port.gpr" -u cubit-sameboy_keys.adb \
        -u cubit-sameboy_frames.adb -u cubit-sameboy_batches.adb \
        --level=2 -j4 --checks-as-errors=on
fi
