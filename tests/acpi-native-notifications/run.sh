#!/usr/bin/env bash
set -euo pipefail
export TMPDIR=/home/doc/cubit-build-tmp
ulimit -s 65536
ulimit -c 0
here=$(cd -- "$(dirname -- "$0")" && pwd)
exec nice -n 19 "${CUBIT_TEST_NODE:-node}" "$here/run.mjs" "$@"
