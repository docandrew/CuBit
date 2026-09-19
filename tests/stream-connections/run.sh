#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../../kernel"
mkdir -p ../tests/stream-connections/build/source
cp ../userspace/runtime/gnat/cubit.ads \
   ../userspace/runtime/gnat/cubit-authority_policy.ads \
   ../userspace/runtime/gnat/cubit-protocols.ads \
   ../userspace/runtime/gnat/cubit-protocols-stream_policies.ads \
   ../userspace/runtime/gnat/cubit-protocols-stream_connections.ads \
   ../userspace/runtime/gnat/cubit-protocols-stream_bindings.ads \
   ../userspace/runtime/gnat/cubit-protocols-stream_bindings.adb \
   ../tests/stream-connections/build/source/
alr exec -- gprbuild -p -P ../tests/stream-connections/connections.gpr
../tests/stream-connections/build/main
../tests/stream-connections/build/lifecycle
alr exec -- gnatprove -P ../tests/stream-connections/connections.gpr \
   -u cubit-protocols-stream_connections.ads proof_cases.adb \
   cubit-protocols-stream_bindings.adb \
   --level=2 --report=all --checks-as-errors=on -j2
