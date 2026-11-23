#!/usr/bin/env bash
set -euo pipefail
here=$(cd -- "$(dirname -- "$0")" && pwd)
: "${TMPDIR:?export TMPDIR=/home/doc/cubit-build-tmp before nix develop}"
if [[ $# -lt 2 ]]; then
    echo "Usage: $0 UPSTREAM_SOURCE OUTPUT_DIRECTORY [run.py options]" >&2
    exit 2
fi
upstream=$1
output=$2
shift 2
mkdir -p -- "$output"
output=$(cd -- "$output" && pwd)
runner=$(python3 "$here/build.py" --output "$output/build")
iasl=$(python3 -c 'import json,sys;print(json.load(open(sys.argv[1]))["tools"]["iasl"]["path"])' "$here/sources.json")
exec python3 "$here/run.py" --upstream "$upstream" --output "$output" --runner "$runner" --iasl "$iasl" "$@"
