#!/usr/bin/env bash
#  Hosted tests of the driver-budget log issuances (Driver_Log_Issuances,
#  ABI-002). --prove also
#  runs gnatprove (level 2) over them, under a memory cap (-j2).
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
links="$here/build/links"
rm -rf "$links"
mkdir -p "$links"
for unit in userspace/lib/logging/driver_log_issuances.ads \
            userspace/lib/logging/driver_log_issuances.adb \
            userspace/runtime/gnat/cubit-log_protocol.ads \
            userspace/runtime/gnat/cubit-log_records.ads userspace/runtime/gnat/cubit-log_records.adb \
            userspace/runtime/gnat/cubit-protocols.ads; do
    ln -s "$root/$unit" "$links/"
done
cd "$root/kernel"
alr exec -- gprbuild -p -q -j8 -P "$here/log_issuances_tests.gpr"
"$here/build/tests/log_issuances_tests"
if [[ ${1:-} == --prove ]]; then
    systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
        alr exec -- gnatprove -P "$here/prove.gpr" -j2 --level=2 --checks-as-errors=on
fi
