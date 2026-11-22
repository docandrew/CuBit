#!/usr/bin/env bash
#  Hosted tests of the launchers' late-introduction ledger (CuBit.Introduction_Ledgers,
#  ABI-002) and the wire encoding (CuBit.Late_Introduction). --prove also
#  runs gnatprove (level 2) over them, under a memory cap (-j2).
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
links="$here/build/links"
rm -rf "$links"
mkdir -p "$links"
for unit in userspace/runtime/gnat/cubit-late_introduction.ads \
            userspace/runtime/gnat/cubit-introduction_ledgers.ads \
            userspace/runtime/gnat/cubit-introduction_ledgers.adb; do
    ln -s "$root/$unit" "$links/"
done
cd "$root/kernel"
alr exec -- gprbuild -p -q -j8 -P "$here/late_introductions_tests.gpr"
"$here/build/tests/late_introductions_tests"
if [[ ${1:-} == --prove ]]; then
    systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
        alr exec -- gnatprove -P "$here/prove.gpr" -j2 --level=2 --checks-as-errors=on
fi
