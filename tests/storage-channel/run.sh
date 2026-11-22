#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../../kernel"
alr exec -- gprbuild -p -P ../tests/storage-channel/channel_tests.gpr
../tests/storage-channel/build/channel_tests
../tests/storage-channel/build/native_bridge_tests
