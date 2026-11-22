#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../../kernel"
alr exec -- gprbuild -p -P ../tests/ccl-objects/schemas.gpr
../tests/ccl-objects/build/schemas/schema_tests
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P ../tests/ccl-objects/schemas.gpr \
        -u ccl-objects-schemas.adb --level=2 -j2 \
        2>&1 | tee ../tests/ccl-objects/build/schemas/proof.log
    if grep -Eq ': (low|medium|high):' ../tests/ccl-objects/build/schemas/proof.log; then exit 1; fi
fi
