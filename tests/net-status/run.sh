#!/usr/bin/env bash
#  Hosted tests for the Deskbar's network and load items (UI-027); --prove
#  also runs gnatprove (level 2) under a memory cap. Run in the Nix shell
#  from anywhere.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/net_status_tests.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    systemd-run --user --scope -q -p MemoryMax=24G -p MemorySwapMax=0 \
        alr exec -- gnatprove -P "$here/net_status_tests.gpr" -j2 \
        -u desktop_net_address.adb -u desktop_network_model.adb \
        -u desktop_address_editor.adb -u desktop_load_bars.adb -u desktop_network_rows.adb -u desktop_status_machines.adb \
        -u cubit-wifi_report.adb -u cubit-net_manager_report.adb -u cubit-net_manager_control.ads
fi
