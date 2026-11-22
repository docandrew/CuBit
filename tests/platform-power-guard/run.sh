#!/usr/bin/env bash
#  Hosted tests of the platform power port guard (KERN-004); --prove also
#  runs gnatprove at level 2. Run in the Nix shell from anywhere.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/platform_power_guard_tests.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/platform_power_guard_tests.gpr" \
        -u platform_power_port_guard.adb --level=2 -j2 --checks-as-errors=on
fi
