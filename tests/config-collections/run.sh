#!/usr/bin/env bash
# Hosted, isolated outputs. Invoke through nix develop -c bash.
set -euo pipefail
cd "$(dirname "$0")/../../kernel"
alr exec -- gprbuild -p -P ../tests/config-collections/collections.gpr
../tests/config-collections/build/collection_tests
../tests/config-collections/build/typed_store_tests
../tests/config-collections/build/managed_tests
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P ../tests/config-collections/collections.gpr \
        -u config_collections.adb config_authority.adb config_typed_store.adb --level=2 -j2 \
        2>&1 | tee ../tests/config-collections/build/proof.log
    if grep -Eq ': (low|medium|high):' ../tests/config-collections/build/proof.log; then
        exit 1
    fi
fi
