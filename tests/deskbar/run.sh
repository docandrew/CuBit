#!/usr/bin/env bash
#  Hosted tests for the Deskbar's pure units; --prove also runs gnatprove
#  (level 2). Run in the Nix shell from anywhere; run the proof under a
#  memory cap (systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0).
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/deskbar_tests.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/deskbar_tests.gpr" \
        -u desktop_notifications.adb -u desktop_task_groups.adb \
        -u desktop_deskbar_layout.adb -u desktop_status_items.adb -u desktop_tooltips.ads -u desktop_window_placement.adb -j2
fi
