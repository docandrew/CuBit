#!/usr/bin/env bash
set -euo pipefail
exec "${NODE:-node}" "$(dirname "$0")/run.mjs"
