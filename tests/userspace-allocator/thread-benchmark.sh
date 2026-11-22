#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../../kernel"
# The release benchmark library carries CuAlloc's C entry points (cualloc_*).
alr exec -- gprbuild -p -P ../userspace/allocator/allocator_host.gpr
cd ..
mkdir -p tests/userspace-allocator/build
rustc --edition=2024 -D warnings -C opt-level=3 tests/userspace-allocator/thread_bench.rs \
  -L native=userspace/allocator/build/lib -l static=cubit_heap \
  -l dl -o tests/userspace-allocator/build/thread_bench
python3 tests/userspace-allocator/thread-benchmark.py "$@"
