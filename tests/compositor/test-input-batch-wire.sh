#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../.."
# Run inside the pinned Nix development shell. Only these portable runtime
# units belong in the hosted source path; the full runtime shadows host GNAT.
mkdir -p tests/compositor/build/input-batch-wire/source
for unit in cubit.ads cubit-grant_references.ads cubit-desktop_protocol.ads cubit-desktop_protocol.adb; do
    cp "userspace/runtime/gnat/$unit" tests/compositor/build/input-batch-wire/source/
done
gprbuild -q -p -P tests/compositor/input_batch_wire.gpr
tests/compositor/build/input-batch-wire/input_batch_wire_tests
gnatprove -P tests/compositor/input_batch_wire.gpr \
    -u compositor_input_batch_wire.adb cubit-desktop_protocol.adb --level=2 --timeout=20 \
    --checks-as-errors=on --report=all -j2
