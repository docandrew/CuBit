#!/usr/bin/env bash
# Hosted transport fault model, not a live kernel-completion authentication test.
set -euo pipefail
cd "$(dirname "$0")/../../kernel"
alr exec -- gprbuild -p -P ../tests/config-channel/channel_tests.gpr
../tests/config-channel/build/config_channel_tests
../tests/config-channel/build/receiver_tests
../tests/config-channel/build/type_channel_tests
