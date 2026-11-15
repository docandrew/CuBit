#!/usr/bin/env bash
set -euo pipefail
export TMPDIR=/home/doc/cubit-build-tmp
exec "${NODE_BIN:-node}" "$(dirname "$0")/run.mjs" "$@"
