#!/usr/bin/env bash
set -euo pipefail
: "${TMPDIR:?Export TMPDIR=/home/doc/cubit-build-tmp before Nix}"
ulimit -c 0
ulimit -s 65536
exec node "$(dirname "$0")/run.mjs" "$@"
