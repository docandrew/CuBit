#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../../kernel"
alr exec -- gprbuild -p -P ../tests/ccl-objects/schema_codec.gpr
../tests/ccl-objects/build/schema-codec/schema_codec_tests
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P ../tests/ccl-objects/schema_codec.gpr \
        -u ccl-objects-schemas-persistence.adb --level=2 -j2 \
        2>&1 | tee ../tests/ccl-objects/build/schema-codec/proof.log
    if grep -Eq ': (low|medium|high):' ../tests/ccl-objects/build/schema-codec/proof.log; then exit 1; fi
fi
