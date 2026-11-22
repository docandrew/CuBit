#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../../kernel"
alr exec -- gprbuild -p -P ../tests/config-object-client/client.gpr
../tests/config-object-client/build/client_tests
../tests/config-object-client/build/dispatch_tests
../tests/config-object-client/build/receiver_tests
../tests/config-object-client/build/startup_tests
alr exec -- gprbuild -p -P ../tests/config-object-client/resource_client.gpr
../tests/config-object-client/build/resource-client/resource_client_tests
alr exec -- gprbuild -p -P ../tests/config-object-client/resources.gpr
../tests/config-object-client/build/resources/resource_tests
../tests/config-object-client/build/resources/resource_vm_tests
alr exec -- gprbuild -p -P ../tests/config-object-client/vm_client.gpr
../tests/config-object-client/build/vm/vm_tests
alr exec -- gprbuild -p -P ../tests/config-object-client/host_client.gpr
../tests/config-object-client/build/host/host_tests
../tests/config-object-client/build/host/read_outcome_tests
../tests/config-object-client/build/host/view_tests
../tests/config-object-client/build/host/read_source_tests
alr exec -- gprbuild -p -P ../tests/config-object-client/calls.gpr
../tests/config-object-client/build/calls/call_tests
../tests/config-object-client/build/calls/outcome_tests
../tests/config-object-client/build/calls/native_call_tests
python3 ../tests/config-object-client/test-outcome-schema.py
alr exec -- gprbuild -p -P ../tests/config-object-client/resource_calls.gpr
../tests/config-object-client/build/resource-calls/resource_call_tests
alr exec -- gprbuild -p -P ../tests/config-object-client/interfaces.gpr
../tests/config-object-client/build/interfaces/interface_tests
alr exec -- gprbuild -p -P ../tests/config-object-client/runs.gpr
../tests/config-object-client/build/runs/run_tests
python3 ../tests/config-object-client/native-app/test-report-benchmark.py
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P ../tests/config-object-client/client.gpr \
        -u config_object_messages.adb config_object_dispatch.adb config_worker_startup.adb --level=2 -j2 \
        2>&1 | tee ../tests/config-object-client/build/proof.log
    if grep -Eq ': (low|medium|high):' ../tests/config-object-client/build/proof.log; then exit 1; fi
fi
