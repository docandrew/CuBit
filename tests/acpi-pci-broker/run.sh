#!/usr/bin/env bash
set -euo pipefail
: "${TMPDIR:?Export TMPDIR before entering the Nix development environment}"
ulimit -s 65536
ulimit -c 0
exec nice -n 19 node "$(dirname "$0")/run.mjs"
