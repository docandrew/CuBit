#!/usr/bin/env bash
#  Hosted tests of metrics.svc's series store (MET-002); --prove also runs
#  gnatprove (level 2) on the codecs, the sketch and the outlet wire format.
#  Run in the Nix shell; gnatprove under the memory cap (AGENTS.md).
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
mkdir -p "$here/build/source"
cp "$root"/userspace/services/metricsvc/metric_codecs.ad? \
   "$root"/userspace/services/metricsvc/metric_sketches.ad? \
   "$root"/userspace/services/metricsvc/metric_strings.ad? \
   "$root"/userspace/services/metricsvc/metric_series.ad? \
   "$root"/userspace/lib/heap/heap_arrays.ad? "$root"/userspace/lib/heap/heap_growth.ad? \
   "$root"/userspace/runtime/gnat/cubit.ads \
   "$root"/userspace/runtime/gnat/cubit-metric_outlet.ad? \
   "$root"/userspace/runtime/gnat/cubit-program_descriptions.ad? \
   "$root"/userspace/runtime/gnat/cubit-startup_tables.ad? \
   "$root"/userspace/runtime/gnat/cubit-launch_arguments.ad? "$root"/userspace/runtime/gnat/cubit-launch_grants.ad? \
   "$root"/userspace/runtime/gnat/cubit-protocols.ads \
   "$root"/userspace/runtime/gnat/cubit-channel_contracts.ad? \
   "$here/build/source/"
cd "$root/kernel"
alr exec -- gprbuild -p -q -P "$here/metric_store.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/metric_store.gpr" \
        -u metric_codecs.adb -u metric_sketches.adb -u cubit-metric_outlet.adb \
        --level=2 --checks-as-errors=on -j2
fi
