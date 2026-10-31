#!/usr/bin/env bash
set -euo pipefail
exec node "$(dirname "$0")/check_current_proof.mjs" "$@"
