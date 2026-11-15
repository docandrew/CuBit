#!/usr/bin/env bash
#  Hosted tests of the root stop's pure parts (PWR-003 slice 1): procmgr's
#  stop order and outcomes (Shutdown_Sequences), power.svc's decisions
#  (Power_Decisions) and the wire codecs (CuBit.System_Stops,
#  CuBit.Power_Requests, CuBit.Power_Event_Exchange). --prove also runs
#  gnatprove (level 2) over them, under a memory cap (-j2).
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
links="$here/build/links"
rm -rf "$links"
mkdir -p "$links"
for unit in userspace/runtime/gnat/cubit-system_stops.ads \
            userspace/runtime/gnat/cubit-power_requests.ads \
            userspace/runtime/gnat/cubit-power_event_exchange.ads \
            userspace/runtime/gnat/cubit-close_policies.ads \
            userspace/services/procmgr/shutdown_sequences.ads userspace/services/procmgr/shutdown_sequences.adb \
            userspace/services/power/power_decisions.ads userspace/services/power/power_decisions.adb; do
    ln -s "$root/$unit" "$links/"
done
cd "$root/kernel"
alr exec -- gprbuild -p -q -j8 -P "$here/shutdown_tests.gpr"
"$here/build/tests/shutdown_tests"
if [[ ${1:-} == --prove ]]; then
    systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
        alr exec -- gnatprove -P "$here/prove.gpr" -j2 --level=2 --checks-as-errors=on
fi
