#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../../kernel"
alr exec -- gprbuild -q -p -P ../tests/observatory-metrics/history.gpr
../tests/observatory-metrics/build/history/history_tests
alr exec -- gnatprove -P ../tests/observatory-metrics/history.gpr -u observatory_history.adb --level=2 --report=all --checks-as-errors=on -j1
