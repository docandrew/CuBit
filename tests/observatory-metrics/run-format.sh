#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../../kernel"
alr exec -- gprbuild -q -p -P ../tests/observatory-metrics/format.gpr
../tests/observatory-metrics/build/format/format_tests
alr exec -- gnatprove -P ../tests/observatory-metrics/format.gpr -u observatory_format_budget.adb --level=2 --report=all --checks-as-errors=on -j1
