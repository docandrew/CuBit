#!/usr/bin/env bash
#  Hosted tests for the libc's Ada (docs/c-removal.md); --prove also runs
#  gnatprove. Run in the Nix shell from anywhere.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/libc_ada.gpr"
"$here/build/main"
python3 "$here/check_constants.py"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/libc_ada.gpr" -u cubit-child_table.adb \
        -u cubit-libc_time.adb -u cubit-libc_select.adb -u cubit-libc_reports.adb \
        -u cubit-libc_start_layout.adb -u cubit-libc_rings.adb \
        -u cubit-libc_directory_entries.adb -u cubit-libc_descriptor_rules.adb \
        -u cubit-libc_file_cache.adb -u cubit-libc_dirty_map.adb -u cubit-libc_park_table.adb \
        -u cubit-libc_net_addresses.adb -u cubit-libc_net_targets.adb -u cubit-libc_net_names.adb \
        --level=2 -j4 --checks-as-errors=on
fi
