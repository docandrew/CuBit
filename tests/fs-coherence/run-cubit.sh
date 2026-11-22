#!/usr/bin/env bash
# Every fs-coherence case in the guest (KVM), RUNS times each (default 1),
# serial logs kept in LOG_DIR (default: a new directory under TMPDIR).
# Run inside nix develop, under coordination/build.lock, after
# tests/fs-coherence/build-cubit.sh:
#   flock coordination/build.lock nix develop -c tests/fs-coherence/run-cubit.sh [RUNS]
set -uo pipefail
here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
runs=${1:-1}
logs=${LOG_DIR:-$(mktemp -d "${TMPDIR:-/tmp}/fs-coherence-logs.XXXXXX")}
mkdir -p "$logs"
failed=0
while read -r name <&3; do
    for run in $(seq 1 "$runs"); do
        log=$logs/$name-$run.serial
        if "$root/tests/headless/run.sh" --test "fs-coherence-$name" --accel kvm \
               --timeout 60 --serial "$log" < /dev/null > "$log.out" 2>&1; then
            echo "fs-coherence-$name run $run: PASS"
        else
            echo "fs-coherence-$name run $run: FAIL ($(grep -a 'fs-coherence: .* FAIL' "$log" | head -1))"
            failed=1
        fi
    done
done 3< "$here/cases.txt"
echo "logs: $logs"
exit "$failed"
