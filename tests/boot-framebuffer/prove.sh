#!/usr/bin/env bash
set -euo pipefail
test_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
cd "$test_dir/../../kernel"
alr exec -- gnatprove -P "$test_dir/framebuffer_tests.gpr" \
    -u boot_framebuffer.adb --mode=all --level=1 \
    --prover=cvc5,z3 --checks-as-errors=on -j2 "$@"
