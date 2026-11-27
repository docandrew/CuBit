#!/usr/bin/env bash
set -euo pipefail
here=$(cd -- "$(dirname -- "$0")" && pwd)
cd "$here"
timeout --kill-after=5s 300s gprbuild -p -j2 -P checked.gpr
timeout --kill-after=2s 20s ./build-checked/bulk_ledger_tests
timeout --kill-after=2s 20s ./build-checked/dense_index_tests
timeout --kill-after=2s 20s ./build-checked/dynamic_lifecycle_tests
timeout --kill-after=2s 20s ./build-checked/unavailable_generation_tests
timeout --kill-after=5s 360s python3 -u "$here/run-phase.py"
