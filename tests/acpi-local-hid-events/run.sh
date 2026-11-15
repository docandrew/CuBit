#!/usr/bin/env bash
set -euo pipefail
export TMPDIR=/home/doc/cubit-build-tmp
ulimit -c 0
ulimit -s 65536
suite_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
exec nice -n 19 "${CUBIT_TEST_NODE:-node}" "$suite_dir/run.mjs" "$@"
