#!/usr/bin/env bash
set -euo pipefail
: "${TMPDIR:?Export TMPDIR before entering Nix}"
ulimit -s 65536
ulimit -c 0
exec systemd-run --user --scope --quiet -p MemoryMax=24G -p MemorySwapMax=0 timeout --kill-after=3s 600s python3 "$(dirname -- "$0")/run.py" "$@"
