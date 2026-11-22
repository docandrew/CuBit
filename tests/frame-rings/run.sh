#!/usr/bin/env bash
#  Hosted tests for Desktop's frame ring to Display (IPC-009): every present
#  answered or failed typed, superseded frames answered, forged answers
#  refused, back pressure. Run in the Nix shell.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
runtime="$here/../../userspace/runtime/gnat"
mkdir -p "$here/build/runtime"
for unit in cubit.ads cubit-request_channels.ads cubit-request_channels.adb \
    cubit-submission_queues.ads cubit-submission_queues.adb cubit-slot_rings.ads cubit-slot_rings.adb cubit-channel_rings.ads cubit-channel_rings.adb \
    cubit-display_protocol.ads cubit-display_protocol.adb cubit-desktop_protocol.ads cubit-desktop_protocol.adb \
    cubit-present_timing.ads cubit-present_timing.adb cubit-grant_references.ads \
    cubit-protocols.ads cubit-channel_contracts.ads cubit-channel_contracts.adb; do
  [ -f "$runtime/$unit" ] && cp -p "$runtime/$unit" "$here/build/runtime/$unit"
done
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/frame_rings.gpr"
"$here/build/main" | tee "$here/build/result.txt"
grep -q '^FRAME-RINGS: PASS' "$here/build/result.txt"
