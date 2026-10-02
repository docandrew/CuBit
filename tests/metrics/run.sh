#!/usr/bin/env bash
# Hosted metrics tests and SPARK proof. Run inside the Nix shell:
#   nix develop -c bash tests/metrics/run.sh [--no-prove]
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
runtime="$here/../../userspace/runtime/gnat"
mkdir -p "$here/build/source"
for unit in cubit.ads cubit-protocols.ads cubit-log_records.ads cubit-log_records.adb \
    cubit-log_protocol.ads cubit-metric_records.ads cubit-metric_records.adb \
    cubit-metric_protocol.ads cubit-metric_batches.ads \
    cubit-metric_batches.adb; do
    cp "$runtime/$unit" "$here/build/source/"
done
cd "$here/../../kernel"
alr exec -- gprbuild -q -p -P "$here/metrics.gpr"
"$here/build/main"
if [[ "${1:-}" != "--no-prove" ]]; then
    alr exec -- gnatprove -P "$here/metrics.gpr" \
        -u cubit-metric_records.adb cubit-metric_batches.adb \
        metric_histograms.adb metric_store.adb \
        --level=1 --report=fail --checks-as-errors=on -j4
fi
