#!/usr/bin/env bash
set -euo pipefail
here=$(cd -- "$(dirname -- "$0")" && pwd)
root=$(cd -- "$here/../.." && pwd)
: "${TMPDIR:?export TMPDIR=/home/doc/cubit-build-tmp before nix develop}"
if [[ $# -lt 2 ]]; then
    echo "Usage: $0 PINNED_UPSTREAM_SOURCE NEW_OUTPUT_DIRECTORY [original run.py options]" >&2
    exit 2
fi
upstream=$1
output=$2
shift 2
for arg in "$@"; do
    case "$arg" in --runner|--runner=*|--iasl|--iasl=*) echo 'This labelled lane requires its verified runner and compiler' >&2; exit 2;; esac
done
mkdir -p -- "$output"
output=$(cd -- "$output" && pwd)
runner=$(python3 "$here/build.py" --root "$root" --output "$output/build")
iasl=$(python3 -c 'import hashlib,json,pathlib,sys;t=json.load(open(sys.argv[1]))["tools"]["iasl"];assert hashlib.sha256(pathlib.Path(t["path"]).read_bytes()).hexdigest()==t["sha256"],"iasl tool drift";print(t["path"])' "$here/sources.json")
exec python3 "$root/tests/acpi-aslts-corpus/run.py" --upstream "$upstream" --output "$output" --runner "$runner" --iasl "$iasl" "$@"
