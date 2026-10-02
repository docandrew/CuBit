#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../.."
mkdir -p tests/observatory-metrics/build/source
for unit in cubit.ads cubit-metric_records.ads cubit-metric_records.adb cubit-metric_protocol.ads; do
 cp "userspace/runtime/gnat/$unit" tests/observatory-metrics/build/source/
done
cd kernel
alr exec -- gprbuild -q -p -P ../tests/observatory-metrics/adapter.gpr
../tests/observatory-metrics/build/adapter/adapter_tests
alr exec -- gnatprove -P ../tests/observatory-metrics/adapter.gpr -u observatory_query_lifetime.adb --level=2 --report=all --checks-as-errors=on -j1
