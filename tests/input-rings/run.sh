#!/usr/bin/env bash
#  Hosted tests for the input source rings (IPC-009): the one-way event
#  channel under back pressure, the consumer's set (order across sources,
#  hot-plug, identities), and Desktop's admission rule. Run in the Nix shell.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
runtime="$here/../../userspace/runtime/gnat"
mkdir -p "$here/build/runtime"
for unit in cubit.ads cubit-process_ids.ads cubit-process_ids.adb cubit-input.ads cubit-input.adb \
    cubit-channel_rings.ads cubit-channel_rings.adb cubit-slot_rings.ads cubit-slot_rings.adb \
    cubit-event_channels.ads cubit-event_channels.adb cubit-protocols.ads \
    cubit-channel_contracts.ads cubit-channel_contracts.adb cubit-input_rings.ads; do
  cp -p "$runtime/$unit" "$here/build/runtime/$unit"
done
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/input_rings.gpr"
"$here/build/main" | tee "$here/build/result.txt"
grep -q '^INPUT-RINGS: PASS' "$here/build/result.txt"
