#!/usr/bin/env bash
#  Hosted model tests for nvme.drv's recovery path (BUG-003). Run inside
#  `nix develop` under the shared build lock.
set -euo pipefail
test_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
cd "$test_dir/../../kernel"
alr exec -- gprbuild -q -p -P "$test_dir/nvme_recovery_tests.gpr"
"$test_dir/build/main"
