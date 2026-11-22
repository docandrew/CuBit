#!/usr/bin/env bash
#  Hosted tests for the typed launch configuration (shared/launch) and the
#  publication layouts (shared/publications); --prove also runs gnatprove
#  at level 2. Run in the Nix shell from anywhere.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/launch_configuration_tests.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/launch_configuration_tests.gpr" \
        -u launch_configuration.adb -u round_trips.adb -u system_publications.ads \
        -u process_layout.ads --level=2 -j2 --checks-as-errors=on
fi
