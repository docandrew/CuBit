#!/usr/bin/env bash
set -euo pipefail
: "${TMPDIR:?export TMPDIR=/home/doc/cubit-build-tmp before Nix}"
[[ "$TMPDIR" == /home/doc/cubit-build-tmp ]]
ulimit -c 0
ulimit -s 65536
exec node "$(dirname -- "$0")/run.mjs" "$@"
