#!/usr/bin/env bash
set -euo pipefail
: "${TMPDIR:?Set TMPDIR before Nix}"
ulimit -s 65536
ulimit -c 0
exec node "$(dirname -- "${BASH_SOURCE[0]}")/reference.mjs" "$@"
