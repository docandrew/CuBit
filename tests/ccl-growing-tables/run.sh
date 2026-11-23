#!/usr/bin/env bash
# Hosted tests of CCL.Growing_Tables (the type registry's storage): growth,
# the ceiling, copies sharing storage, copy on write, in-place appends at
# the tip, and copies on several tasks at once. Run inside nix develop.
set -euo pipefail
cd "$(dirname "$0")/../../kernel"
alr exec -- gprbuild -q -p -P ../tests/ccl-growing-tables/growing_tables_tests.gpr
../tests/ccl-growing-tables/build/main
