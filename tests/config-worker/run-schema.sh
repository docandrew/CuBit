#!/usr/bin/env bash
# Linux-hosted protocol/executor model; run inside Nix.
set -euo pipefail
cd "$(dirname "$0")/../../kernel"
alr exec -- gprbuild -p -P ../tests/config-worker/schema_worker.gpr
../tests/config-worker/build/schema-worker/schema_protocol_tests
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P ../tests/config-worker/schema_worker.gpr \
        -u config_schema_protocol.adb schema_proof.adb --level=2 -j2 \
        2>&1 | tee ../tests/config-worker/build/schema-worker/proof.log
    if grep -Eq ': (low|medium|high):|generic-not-analyzed' ../tests/config-worker/build/schema-worker/proof.log; then exit 1; fi
fi
