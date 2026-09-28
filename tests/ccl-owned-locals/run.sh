#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../../kernel"
alr exec -- gprbuild -p -P ../tests/ccl-owned-locals/owned_locals.gpr
../tests/ccl-owned-locals/build/owned_local_tests
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P ../tests/ccl-owned-locals/owned_locals.gpr \
        -u ccl-vm.adb ccl-ownership-bytecode.adb --level=2 -j2 --checks-as-errors=on
fi
