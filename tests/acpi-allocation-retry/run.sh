#!/usr/bin/env bash
set -euo pipefail
: "${TMPDIR:?export TMPDIR before entering the Nix environment}"
ulimit -s 65536
ulimit -c 0
exec nice -n 19 node "$(dirname "$0")/run.mjs" "$@"
