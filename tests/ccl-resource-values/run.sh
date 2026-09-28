#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../../kernel"
alr exec -- gprbuild -p -P ../tests/ccl-resource-values/resource_values.gpr
../tests/ccl-resource-values/build/resource_value_tests
../tests/ccl-resource-values/build/receiver_tests
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P ../tests/ccl-resource-values/resource_values.gpr \
        -u ccl-vm.adb ccl-vm-resource_values.adb ccl-resources.adb \
        ccl-objects-values.adb ccl-vm-native_objects.adb ccl-host_values.adb \
        --level=2 -j2 --checks-as-errors=on
fi
