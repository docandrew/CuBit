#!/usr/bin/env bash
set -euo pipefail
export TMPDIR=/home/doc/cubit-build-tmp
ulimit -s 65536
ulimit -c 0
exec nice -n 19 node "$(dirname -- "$0")/run.mjs"
