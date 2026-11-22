#!/usr/bin/env bash
set -euo pipefail
: "${TMPDIR:?export TMPDIR=/home/doc/cubit-build-tmp before Nix}"
[[ "$TMPDIR" == /home/doc/cubit-build-tmp ]]
ulimit -s 65536
ulimit -c 0
exec nice -n19 node "$(dirname -- "$0")/run.mjs" "$@"
