#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../../kernel"
alr exec -- gprbuild -p -P ../tests/ccl-schema-catalog/catalog_tests.gpr
../tests/ccl-schema-catalog/build/main
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P ../tests/ccl-schema-catalog/catalog_tests.gpr \
        -u ccl-objects-catalog.adb --level=2 --checks-as-errors=on --report=all -j2
fi
