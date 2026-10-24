#!/usr/bin/env bash
#  Hosted tests for the RTL8852BE driver's SPARK parsers
#  (userspace/services/rtw89/spark); --prove also runs gnatprove at level 2.
#  Run in the Nix shell from anywhere. The firmware-container test reads the
#  vendored linux-firmware blob (firmware/rtw89/rtw8852b_fw-1.bin).
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/rtw89_tests.gpr"
RTW89_FIRMWARE="$here/../../firmware/rtw89/rtw8852b_fw-1.bin" "$here/build/main"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/rtw89_tests.gpr" \
        -u rtw89_efuse.adb -u rtw89_firmware.adb -u rtw89_descriptors.ads --level=2 -j2 --checks-as-errors=on
fi
