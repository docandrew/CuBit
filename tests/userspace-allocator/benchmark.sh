#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../../kernel"
alr exec -- gprbuild -p -P ../userspace/allocator/allocator_host.gpr
cd ..
python3 tests/userspace-allocator/check-codegen.py
mkdir -p tests/userspace-allocator/build
rustc --edition=2024 -D warnings -C opt-level=3 tests/userspace-allocator/bridge_tests.rs \
  -L native=userspace/allocator/build/lib -l static=cubit_heap \
  -o tests/userspace-allocator/build/bridge_tests
tests/userspace-allocator/build/bridge_tests
rustc --edition=2024 -D warnings -C opt-level=3 -C debuginfo=1 tests/userspace-allocator/bench.rs \
  -L native=userspace/allocator/build/lib -l static=cubit_heap -l dl \
  -o tests/userspace-allocator/build/bench
python3 tests/userspace-allocator/benchmark.py "$@"
