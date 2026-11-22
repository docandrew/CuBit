#!/usr/bin/env bash
set -euo pipefail
test_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
cd "$test_dir/../../kernel"
alr exec -- gnatprove -P "$test_dir/buddy_geometry_tests.gpr" \
    -u buddy_geometry.adb --mode=all --level=1 \
    --prover=cvc5,z3 --checks-as-errors=on -j2 "$@"
