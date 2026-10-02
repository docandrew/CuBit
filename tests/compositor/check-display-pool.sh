#!/usr/bin/env bash
# Run inside nix develop; isolate portable units from the freestanding runtime.
set -euo pipefail
compositor_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$compositor_root"
mkdir -p tests/compositor/build/display-pool/source
for unit in cubit.ads cubit-grant_references.ads cubit-desktop_protocol.ads \
            cubit-desktop_protocol.adb cubit-display_protocol.ads cubit-display_protocol.adb; do
    cp "userspace/runtime/gnat/$unit" "tests/compositor/build/display-pool/source/$unit"
done
gprbuild -p -P tests/compositor/display_pool.gpr
tests/compositor/build/display-pool/display_pool_tests
gnatprove -P tests/compositor/display_pool.gpr --level=2 -j1
