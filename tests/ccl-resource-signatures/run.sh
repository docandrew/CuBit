#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../../kernel"
alr exec -- gprbuild -p -P ../tests/ccl-resource-signatures/signatures.gpr
../tests/ccl-resource-signatures/build/signature_tests
../tests/ccl-resource-signatures/build/lowering_tests
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P ../tests/ccl-resource-signatures/signatures.gpr \
        -u ccl-host_values.adb ccl-objects-values.adb ccl-catalog.adb ccl-compiler.adb \
        --subdirs=lowering-proof --level=2 -j2 --checks-as-errors=on
fi
