#!/usr/bin/env bash
set -euo pipefail
: "${TMPDIR:?Set TMPDIR before Nix}"
ulimit -s 65536
ulimit -c 0
exec nice -n19 node "$(dirname "$0")/run.mjs" "$@"
