#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../.."
mkdir -p tests/observatory-metrics/build/source
for unit in cubit.ads cubit-metric_records.ads cubit-metric_records.adb cubit-metric_protocol.ads; do
 cp "userspace/runtime/gnat/$unit" tests/observatory-metrics/build/source/
done
cd kernel
alr exec -- gprbuild -q -p -P ../tests/observatory-metrics/summary.gpr
../tests/observatory-metrics/build/main
alr exec -- gnatprove -P ../tests/observatory-metrics/summary.gpr -u observatory_metric_summaries.adb observatory_metric_queries.adb --level=2 --report=all --checks-as-errors=on -j1
