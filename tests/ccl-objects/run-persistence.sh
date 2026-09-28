#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../../kernel"
alr exec -- gprbuild -p -P ../tests/ccl-objects/persistence.gpr
../tests/ccl-objects/build/persistence/persistence_tests
if [ "${1:-}" = --prove ]; then
    alr exec -- gnatprove -P ../tests/ccl-objects/persistence.gpr \
        -u ccl-objects-persistence.adb --mode=prove --level=2 -j2 \
        2>&1 | tee ../tests/ccl-objects/build/persistence/proof.log
    if grep -Eq ': (low|medium|high):' ../tests/ccl-objects/build/persistence/proof.log; then exit 1; fi
fi
