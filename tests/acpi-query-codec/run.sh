#!/usr/bin/env bash
set -euo pipefail
test -n "$TMPDIR"
ulimit -s 65536
ulimit -c 0
if [[ "${1:-}" == "--verify" && "$#" == 1 ]]; then
  exec node "$(dirname "$0")/verify.mjs"
fi
exec node "$(dirname "$0")/run.mjs" "$@"
