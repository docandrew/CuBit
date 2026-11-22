#!/usr/bin/env bash
# Penny's bytes do not depend on the CuBit commit's time (BLD-004).
#
#     tests/servo/run-build-epoch.sh      (repository root, in nix develop,
#                                          under the build lock)
#
# kernel/Makefile exports SOURCE_DATE_EPOCH from the CuBit HEAD commit;
# Servo's devtools put it into Penny as a build ID, so every unrelated CuBit
# commit used to change cubitshell.app. Penny now takes its epoch from its
# pinned Servo fork commit. This builds Penny (make -C kernel servo) as if
# at two CuBit commits a day apart, the Makefile's epoch set on the command
# line, and requires the same sha256. Fails before the fix (the devtools
# build ID differs), passes after.
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
app=$root/userspace/rust/build/cubitshell.app
mkdir -p "$root/tests/servo/build"
now=$(git -C "$root" log -1 --format=%ct)
declare -a sums
for epoch in "$now" "$((now - 86400))"; do
    make -C "$root/kernel" servo SOURCE_DATE_EPOCH="$epoch" > "$root/tests/servo/build/build-epoch-$epoch.log" 2>&1
    sum=$(sha256sum "$app" | cut -d' ' -f1)
    echo "CuBit epoch $epoch: cubitshell.app $sum"
    sums+=("$sum")
done
if [ "${sums[0]}" = "${sums[1]}" ]; then
    echo "PENNY-BUILD-EPOCH: PASS same bytes at both CuBit commit times"
else
    echo "PENNY-BUILD-EPOCH: FAIL cubitshell.app changes with the CuBit commit time"
    exit 1
fi
