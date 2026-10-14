#!/usr/bin/env bash
#  Hosted tests for CuBit.Startup_Tables (and the startup builder and
#  capability handles); --prove also runs gnatprove (level 2). Run in the
#  Nix shell from anywhere.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/startup_tables.gpr"
"$here/build/main"
for scenario in ready absent built missing wrong-kind wrong-need incomplete malformed not-loaded; do
    "$here/build/main" "$scenario"
done
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/startup_tables.gpr" \
        -u cubit-startup_tables.adb -u cubit-startup_state.adb \
        -u cubit-handles.ads -u startup.ads -u builder_proof.adb \
        -u cubit-launch_envelopes.adb --level=2 -j2 --checks-as-errors=on
fi
