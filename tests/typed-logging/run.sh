#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../../kernel"
mkdir -p ../tests/typed-logging/build/source
for unit in cubit.ads cubit-protocols.ads \
    cubit-log_records.ads cubit-log_records.adb \
    cubit-text_to_log.ads cubit-text_to_log.adb \
    cubit-authority_policy.ads cubit-protocols-stream_policies.ads \
    cubit-protocols-stream_connections.ads \
    cubit-protocols-stream_bindings.ads cubit-protocols-stream_bindings.adb; do
    cp "../userspace/runtime/gnat/$unit" ../tests/typed-logging/build/source/
done
alr exec -- gprbuild -p -P ../tests/typed-logging/logging.gpr
../tests/typed-logging/build/main
../tests/typed-logging/build/demo
alr exec -- gnatprove -P ../tests/typed-logging/logging.gpr \
    -u cubit-log_records.adb cubit-text_to_log.adb \
    --level=2 --report=all --checks-as-errors=on -j2
