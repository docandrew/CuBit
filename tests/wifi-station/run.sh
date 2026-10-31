#!/usr/bin/env bash
#  Hosted tests for the chip-independent Wi-Fi station units
#  (userspace/lib/wifi; NET-WIFI-001 W3/W4) and the wifi.svc <-> driver
#  contract; --prove also runs gnatprove (level 2) over them. Run in the
#  Nix shell from anywhere.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here"
gprbuild -p -q -j0 -P wifi_station_tests.gpr
./build/station_tests
if [[ ${1:-} == --prove ]]; then
    systemd-run --user --scope -q -p MemoryMax=24G -p MemorySwapMax=0 \
        gnatprove -P wifi_station_tests.gpr -j2 --output=oneline \
        -u wifi_rsn.adb -u wifi_eapol.adb -u wifi_key_data.adb -u wifi_handshake.adb \
        -u ieee80211_management.adb -u wifi_mlme.adb -u cubit-wifi_driver.adb -u ieee80211_data.adb \
        -u cubit-wifi_control.adb -u wifi_station.adb -u wifi_replay.adb -u wifi_station_path.adb
fi
