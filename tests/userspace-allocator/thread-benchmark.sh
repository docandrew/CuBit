#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../.."
mkdir -p tests/userspace-allocator/build
rustc --edition=2024 -D warnings -C opt-level=3 tests/userspace-allocator/thread_bench.rs \
  -l dl -o tests/userspace-allocator/build/thread_bench
python3 tests/userspace-allocator/thread-benchmark.py "$@"
