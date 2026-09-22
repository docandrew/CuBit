#!/usr/bin/env bash
set -eu
root=$(cd "$(dirname "$0")/../.." && pwd)
mkdir -p "$root/tests/performance/build/src" "$root/tests/performance/build/obj"
cp "$root/userspace/runtime/gnat/cubit.ads" \
   "$root/userspace/runtime/gnat/cubit-timing_histograms.ads" \
   "$root/userspace/runtime/gnat/cubit-timing_histograms.adb" \
   "$root/userspace/runtime/gnat/cubit-graphics_metrics.ads" \
   "$root/userspace/runtime/gnat/cubit-graphics_metrics.adb" \
   "$root/tests/performance/build/src/"
cd "$root/kernel"
alr exec -- gprbuild -P ../tests/performance/histogram.gpr
../tests/performance/build/histogram_test
../tests/performance/build/graphics_counter_test
python3 -m unittest discover -s ../tests/performance -v
