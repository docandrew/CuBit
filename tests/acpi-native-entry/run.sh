#!/usr/bin/env bash
set -euo pipefail
export TMPDIR=/home/doc/cubit-build-tmp
ulimit -s 65536
ulimit -c 0
exec nice -n 19 "${CUBIT_TEST_NODE:-node}" "$(dirname -- "${BASH_SOURCE[0]}")/run.mjs" "$@"
