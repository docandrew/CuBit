#!/usr/bin/env bash
set -euo pipefail
export TMPDIR=${TMPDIR:?Set TMPDIR before entering Nix}
ulimit -s 65536
ulimit -c 0
exec node "$(dirname -- "${BASH_SOURCE[0]}")/run.mjs" "$@"
