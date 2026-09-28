#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../../kernel"
alr exec -- gprbuild -p -P ../tests/ccl-objects/objects.gpr
../tests/ccl-objects/build/object_tests
../tests/ccl-objects/build/value_tests
../tests/ccl-objects/build/correspondence_tests
../tests/ccl-objects/build/durable_tests
if [ "${1:-}" = --prove ]; then
    alr exec -- gnatprove -P ../tests/ccl-objects/objects.gpr \
        -u ccl-objects.adb ccl-objects-values.adb ccl-types-correspondence.adb config_objects.adb --mode=prove --level=2 -j2 \
        2>&1 | tee ../tests/ccl-objects/build/proof.log
    if grep -Eq ': (low|medium|high):' ../tests/ccl-objects/build/proof.log; then exit 1; fi
fi
