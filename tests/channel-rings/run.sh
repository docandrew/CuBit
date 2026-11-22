#!/usr/bin/env bash
#  Host tests for CuBit.Channel_Rings; --prove also runs gnatprove (level 1).
set -euo pipefail
cd "$(dirname "$0")/../../kernel"
alr exec -- gprbuild -p -q -P ../tests/channel-rings/rings.gpr
../tests/channel-rings/build/main
python3 ../tests/channel-rings/layout-check.py
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P ../tests/channel-rings/rings.gpr \
        -u cubit-channel_rings.adb -u cubit-datagram_rings.adb -u cubit-stream_rings.adb \
        -u slot_ring_small.ads -u slot_ring_frames.ads -u cubit-frame_rings.ads -u queue_small.ads --level=1 -j2 --checks-as-errors=on
fi
