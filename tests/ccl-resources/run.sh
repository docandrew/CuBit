#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../../kernel"
alr exec -- gprbuild -p -P ../tests/ccl-resources/resources.gpr
../tests/ccl-resources/build/resource_tests
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P ../tests/ccl-resources/resources.gpr \
        -u ccl-resources.adb --level=2 -j2 --checks-as-errors=on
fi
