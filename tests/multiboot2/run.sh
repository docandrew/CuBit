#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../../kernel"
alr exec -- gprbuild -p -P ../tests/multiboot2/boot_tests.gpr
../tests/multiboot2/build/main
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P ../tests/multiboot2/boot_tests.gpr \
        -u multiboot2_info.adb --mode=all --level=2 --prover=cvc5,z3 \
        --checks-as-errors=on -j2 2>&1 | tee ../tests/multiboot2/build/proof.log
fi
