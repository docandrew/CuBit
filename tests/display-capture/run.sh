#!/usr/bin/env bash
#  Hosted tests for remote screenshots (REMOTE-001): Desktop's capture
#  policy and the display-capture wire; --prove also runs gnatprove
#  (level 2) under a memory cap. Run in the Nix shell from anywhere.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/display_capture_tests.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    systemd-run --user --scope -q -p MemoryMax=24G -p MemorySwapMax=0 \
        alr exec -- gnatprove -P "$here/display_capture_tests.gpr" -j2 \
        -u desktop_capture_policy.adb -u cubit-display_capture.ads
fi
