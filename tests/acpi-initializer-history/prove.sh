#!/usr/bin/env bash
set -euo pipefail
exec node "$(dirname "$0")/proof/run.mjs" "$@"
