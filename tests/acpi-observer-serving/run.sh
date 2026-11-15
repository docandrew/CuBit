#!/usr/bin/env bash
set -euo pipefail
node "$(dirname "$0")/run.mjs" "$@"
exec node "$(dirname "$0")/retirement.mjs"
