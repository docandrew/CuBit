#!/usr/bin/env bash
set -euo pipefail
if [[ "${1-}" == --prove ]]; then shift; exec node "$(dirname "$0")/prove.mjs" "$@"; fi
exec node "$(dirname "$0")/run.mjs" "$@"
