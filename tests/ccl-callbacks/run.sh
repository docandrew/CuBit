#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../../kernel"
alr exec -- gprbuild -p -P ../tests/ccl-callbacks/callbacks_tests.gpr
../tests/ccl-callbacks/build/queue_tests
../tests/ccl-callbacks/build/handler_tests
../tests/ccl-callbacks/build/button_tests
