#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../../kernel"
alr exec -- gprbuild -p -P ../tests/ccl-resource-policies/policies.gpr
../tests/ccl-resource-policies/build/policy_tests
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P ../tests/ccl-resource-policies/policies.gpr \
        -u ccl-types.adb ccl-resource_policies.adb ccl-catalog.adb \
        --level=2 -j2 --checks-as-errors=on
fi
