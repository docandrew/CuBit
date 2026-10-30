#!/usr/bin/env bash
set -euo pipefail
test "${TMPDIR:-}" = /home/doc/cubit-build-tmp
ulimit -c 0
ulimit -s 65536
exec node "$(dirname "$0")/run.mjs" "$@"
