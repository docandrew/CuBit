#!/usr/bin/env bash
set -euo pipefail
export TMPDIR="${TMPDIR:?set TMPDIR before entering Nix}"
ulimit -s 65536
ulimit -c 0
cd -- "$(dirname -- "${BASH_SOURCE[0]}")"
exec node reference-ordering.mjs
