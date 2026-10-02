#!/usr/bin/env bash
# Nix-hosted checks; assertions are not enabled in native images.
set -euo pipefail
cd "$(dirname "$0")/../../kernel"
alr exec -- gprbuild -p -P ../tests/ccl-type-discovery/discovery.gpr
../tests/ccl-type-discovery/build/import_tests
../tests/ccl-type-discovery/build/catalog_tests
../tests/ccl-type-discovery/build/metadata_tests
../tests/ccl-type-discovery/build/function_tests
alr exec -- gprbuild -p -P ../tests/ccl-type-discovery/host_objects.gpr
../tests/ccl-type-discovery/build/host-objects/host_object_tests
alr exec -- gprbuild -p -P ../tests/ccl-type-discovery/import_results.gpr
../tests/ccl-type-discovery/build/import-results/import_result_tests
alr exec -- gprbuild -p -P ../tests/ccl-type-discovery/portable_objects.gpr
../tests/ccl-type-discovery/build/portable-objects/portable_object_tests
alr exec -- gprbuild -p -P ../tests/ccl-type-discovery/native_objects.gpr
../tests/ccl-type-discovery/build/native-objects/native_object_tests
alr exec -- gprbuild -p -P ../tests/ccl-type-discovery/log_view.gpr
../tests/ccl-type-discovery/build/log-view/log_view_tests
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P ../tests/ccl-type-discovery/discovery.gpr \
        -u ccl-types.adb --level=2 -j2 \
        2>&1 | tee ../tests/ccl-type-discovery/build/proof.log
    if grep -Eq ': (low|medium|high):' ../tests/ccl-type-discovery/build/proof.log; then exit 1; fi
    alr exec -- gnatprove -P ../tests/ccl-type-discovery/native_objects.gpr \
        -u ccl-vm.adb ccl-vm-native_objects.adb ccl-host_values.adb \
        --subdirs=object-projection-proof --level=2 -j2 --checks-as-errors=on \
        2>&1 | tee ../tests/ccl-type-discovery/build/native-objects/proof.log
fi
