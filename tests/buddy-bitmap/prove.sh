#!/usr/bin/env bash
set -euo pipefail

test_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
cd "$test_dir/../../kernel"
# GNATprove 15 requires an absolute path for this additional configuration.
# Keep the default encoding and add CVC5's integer encoding for the mixed
# bitvector/integer bridge. Both prove actual obligations; neither waives them.
alr exec -- gnatprove -P "$test_dir/buddy_bitmap_tests.gpr" \
    -u buddy_bitmap.adb --mode=all --level=1 \
    --why3-conf="$test_dir/why3.conf" --prover=cvc5,cvc5_int,z3 \
    --checks-as-errors=on -j2 "$@"
