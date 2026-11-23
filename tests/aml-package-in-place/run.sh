#!/usr/bin/env bash
set -euo pipefail
: "${TMPDIR:?Export TMPDIR before entering Nix}"
ulimit -s 131072
ulimit -c 0
exec node "$(dirname -- "$0")/run.mjs" "$@"
