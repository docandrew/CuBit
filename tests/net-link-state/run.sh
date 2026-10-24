#!/usr/bin/env bash
#  Hosted tests for the link contract and netstack's link state machine;
#  --prove also runs gnatprove (level 2) over the units under test.
set -euo pipefail
cd "$(dirname "$0")/../../kernel"
alr exec -- gprbuild -p -q -P ../tests/net-link-state/net_link_state.gpr
../tests/net-link-state/build/main
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P ../tests/net-link-state/net_link_state.gpr \
        -u cubit-frame_rings.ads -u cubit-net_link.adb -u cubit-net_management.adb -u link_tracker.adb \
        --level=2 -j2 --checks-as-errors=on
fi
