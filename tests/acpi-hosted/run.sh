#!/usr/bin/env bash
set -euo pipefail
script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../.." && pwd)"
export TMPDIR=/home/doc/cubit-build-tmp
cd "$repo_root"
if [[ "${ACPI_HOSTED_USE_CURRENT_SHELL:-0}" == 1 ]]; then
  exec nice -n 19 python3 "$script_dir/run.py" "$@"
fi
exec nice -n 19 nix develop "path:$repo_root" -c python3 "$script_dir/run.py" "$@"
