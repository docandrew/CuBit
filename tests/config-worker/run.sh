#!/usr/bin/env bash
# Linux-hosted only. Run inside the Nix shell; no native staging or boot edits.
set -euo pipefail
cd "$(dirname "$0")/../../kernel"
alr exec -- gprbuild -p -P ../tests/config-worker/worker.gpr
../tests/config-worker/build/protocol_tests
../tests/config-worker/build/execution_tests
if [ "${1:-}" = --prove ]; then
    # The concrete instance is essential: a generic template is not analyzed.
    alr exec -- gnatprove -P ../tests/config-worker/worker.gpr \
        -u worker_proof.adb config_worker_protocol.adb --mode=prove --level=2 -j2 \
        2>&1 | tee ../tests/config-worker/build/proof.log
    if grep -Eq ': (low|medium|high):|generic-not-analyzed' ../tests/config-worker/build/proof.log; then
        exit 1
    fi
fi
