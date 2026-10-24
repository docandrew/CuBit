#!/usr/bin/env bash
#  Hosted tests for netmgr's DHCP client and its typed interface setting;
#  --prove also runs gnatprove (level 2) over the DHCP client.
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
cd "$root/kernel"
alr exec -- gprbuild -p -q -P "$here/netmgr_dhcp.gpr"
"$here/build/main"
mkdir -p "$here/build/protocol"
for unit in cubit-settings.ads cubit-settings.adb cubit-settings-codecs.ads cubit-settings-codecs.adb \
            cubit-config_protocol.ads cubit-config_protocol.adb cubit-locators.ads cubit-locators.adb cubit-net_address.ads \
            cubit-net_link.ads cubit-net_link.adb cubit-net_management.ads cubit-net_management.adb \
            cubit-frame_rings.ads; do
    cp "$root/userspace/runtime/gnat/$unit" "$here/build/protocol/"
done
alr exec -- gprbuild -p -q -j8 -P "$here/settings.gpr" \
    --relocate-build-tree="$here/build/tree" --root-dir="$root"
"$here/build/tree/tests/netmgr-dhcp/build/settings/settings_tests"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/netmgr_dhcp.gpr" -u dhcp_client.adb \
        --level=2 -j2 --checks-as-errors=on
fi
