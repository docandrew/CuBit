#!/usr/bin/env bash
set -euo pipefail
: "${TMPDIR:?Set TMPDIR before Nix}"
ulimit -c 0
ulimit -s 65536
exec node "$(dirname "$0")/run.mjs" "$@"
