#!/usr/bin/env bash
#  Hosted tests for the RTL8852BE driver's SPARK units
#  (userspace/services/rtw89/spark) and the scan contract (CuBit.Wifi_Scan):
#  unit checks, then differential tests against independent Python
#  decoders (reference.py) on constructed, recorded and mutated inputs.
#  Run in the Nix shell from anywhere.
#
#  Environment (all optional):
#    RTW89_PCAPS   space-separated pcap files whose beacons and probe
#                  responses seed the frame corpus (recorded frames); the
#                  Wireshark sample captures wpa-Induction.pcap (SHA-256
#                  2b57dca7...) and Network_Join_Nokia_Mobile.pcap
#                  (8c65e7a8...) were used for W1.
#    RTW89_TABLE_SOURCE  Linux rtw8852b_table.c at the pinned commit: the
#                  table check then reads it directly (else the generated
#                  Ada spec), and the generator's output is re-checked.
#  --prove also runs gnatprove (level 2) over the SPARK units.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
work="$here/build/differential"
mkdir -p "$work"
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/rtw89_tests.gpr"
RTW89_FIRMWARE="$here/../../firmware/rtw89/rtw8852b_fw-1.bin" "$here/build/main"

ref="python3 -I $here/reference.py"
# shellcheck disable=SC2086
$ref corpus "$work/frames.hex" ${RTW89_PCAPS:-}
"$here/build/main" frames "$work/frames.hex" "$work/frames.ada"
$ref frames "$work/frames.hex" "$work/frames.py"
$ref compare "$work/frames.ada" "$work/frames.py"

$ref rxcorpus "$work/rx.hex"
"$here/build/main" rx "$work/rx.hex" "$work/rx.ada"
$ref rx "$work/rx.hex" "$work/rx.py"
$ref compare "$work/rx.ada" "$work/rx.py"

source_file="${RTW89_TABLE_SOURCE:-$here/../../userspace/services/rtw89/spark/rtw89_tables_8852b.ads}"
"$here/build/main" tables "$work/tables.ada"
$ref tables "$source_file" "$work/tables.py"
$ref compare "$work/tables.ada" "$work/tables.py"
if [[ -n ${RTW89_TABLE_SOURCE:-} ]]; then
    python3 -I "$here/../../userspace/services/rtw89/tools/generate_tables.py" --check \
        "$RTW89_TABLE_SOURCE" "$here/../../userspace/services/rtw89/spark/rtw89_tables_8852b.ads"
fi

if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/rtw89_tests.gpr" \
        -u rtw89_efuse.adb -u rtw89_firmware.adb -u rtw89_descriptors.ads \
        -u cubit-wifi_scan.adb -u rtw89_rx.adb -u ieee80211_beacons.adb -u wifi_bss_table.adb \
        -u wifi_channel_plan.ads -u rtw89_phy_tables.adb -u rtw89_h2c.adb -u rtw89_channel_values.ads \
        --level=2 -j2 --checks-as-errors=on
fi
