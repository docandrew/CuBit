#!/usr/bin/env bash
set -euo pipefail
test "${TMPDIR:-}" = /home/doc/cubit-build-tmp
ulimit -s 65536
ulimit -c 0
exec node "$(dirname "$0")/run.mjs" "$@"
