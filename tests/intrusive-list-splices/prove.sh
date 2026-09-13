#!/usr/bin/env bash
set -euo pipefail
test_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
cd "$test_dir/../../kernel"
alr exec -- gnatprove -P "$test_dir/splice_tests.gpr" \
    -u test_splices.ads address_splices.ads buddy_list_refinement.adb buddy_blocks.adb \
    --mode=all --level=1 --prover=cvc5,z3 --checks-as-errors=on -j2 "$@"
