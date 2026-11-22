#!/usr/bin/env bash
# CuAlloc's routing (binary-search free) and miss-scan costs against the
# references; see scaling_bench.rs. Hold coordination/build.lock for a run.
set -euo pipefail
cd "$(dirname "$0")/../../kernel"
alr exec -- gprbuild -p -P ../userspace/allocator/allocator_host.gpr
cd ..
mkdir -p tests/userspace-allocator/build
rustc --edition=2024 -D warnings -C opt-level=3 tests/userspace-allocator/scaling_bench.rs \
  -L native=userspace/allocator/build/lib -l static=cubit_heap \
  -l dl -o tests/userspace-allocator/build/scaling_bench
python3 tests/userspace-allocator/scaling-benchmark.py "$@"
