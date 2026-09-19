#!/usr/bin/env bash
set -euo pipefail
test_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
cd "$test_dir/../../kernel"
alr exec -- gprbuild -P "$test_dir/framebuffer_tests.gpr"
"$test_dir/build/main"
