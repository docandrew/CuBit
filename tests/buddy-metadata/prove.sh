#!/usr/bin/env bash
set -euo pipefail
test_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
cd "$test_dir/../../kernel"
alr exec -- gnatprove -P "$test_dir/metadata_tests.gpr" \
    -u buddy_metadata.adb --mode=all --level=1 \
    --why3-conf="$test_dir/../buddy-bitmap/why3.conf" \
    --prover=cvc5,cvc5_int,z3 --checks-as-errors=on -j2 "$@"
