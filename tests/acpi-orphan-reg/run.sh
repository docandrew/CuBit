#!/usr/bin/env bash
set -euo pipefail
ulimit -s 65536
ulimit -c 0
exec "${NODE_BIN:-node}" "$(dirname -- "$0")/run.mjs" "$@"
