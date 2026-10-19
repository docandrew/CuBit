#!/usr/bin/env bash
#  Hosted tests for devmgr's boot RTC seed; --prove also runs gnatprove
#  (level 2) on RTC_Decode. Run in the Nix shell; run the proof under a
#  memory cap (systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0).
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/boot_rtc_tests.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/boot_rtc_tests.gpr" -u rtc_decode.adb -j2
fi
