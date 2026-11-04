#!/usr/bin/env bash
# Liveness of step claims (docs/ccl-build.md, "The Storehouse"): a step runs
# under its flock lock, which the kernel releases when the holder dies, so
#   - a builder killed (SIGKILL, by its PID) in the middle of a step does not
#     block the next one, which removes what it left and runs the step;
#   - a stale claim whose process number now belongs to a live, unrelated
#     process (PID reuse) is recognised as stale (before: "the step is
#     running in process N" until that process exits);
#   - a claim placed by an older ccl-build without locks, naming a live
#     process, is still respected (that build may really be running);
#   - work directories of dead processes are removed, of live ones kept.
# Run inside `nix develop` with TMPDIR set: bash tests/ccl-build/liveness.sh
set -euo pipefail

here=$(cd "$(dirname "$0")" && pwd)
driver=$here/build/store_driver
: "${TMPDIR:?set TMPDIR outside /tmp}"
scratch=$(mktemp -d "$TMPDIR/ccl-build-liveness.XXXXXX")
pids=()
cleanup() {
    for p in "${pids[@]}"; do kill "$p" 2>/dev/null || true; done
    chmod -R u+w "$scratch" 2>/dev/null || true; rm -rf "$scratch"
}
trap cleanup EXIT
passed=0; failed=0
check() {
    local name=$1; shift
    if "$@"; then passed=$((passed + 1)); echo "PASS $name"
    else failed=$((failed + 1)); echo "FAIL $name"; fi
}
fails() { ! "$@"; }
store=$scratch/store
sources=$scratch/sources
mkdir -p "$sources"
printf '#!/bin/sh\n/bin/sleep "$1"\n/bin/cat in.txt > a.txt\n/bin/cat in.txt in.txt > b.txt\necho done > c.txt\n' >"$sources/three.sh"
chmod +x "$sources/three.sh"
step() { "$driver" step "$store" "$sources" "${1:-0}"; }

echo "== a builder killed in the middle of a step"
echo first >"$sources/in.txt"
shape=$(STORE_DRIVER_SHAPE=only step 5 | awk '/^shape/ {print $2}')
shape0=$(STORE_DRIVER_SHAPE=only step 0 | awk '/^shape/ {print $2}')
echo second >"$sources/in.txt"            # the same step shape, another input
"$driver" step "$store" "$sources" 5 >"$scratch/victim.out" 2>&1 &   # not through a function: $! must be the builder
victim=$!; pids+=("$victim")
for _ in $(seq 1 100); do [[ -d $store/work/step-$shape ]] && break; sleep 0.1; done
check "the victim is running the step" test -d "$store/work/step-$shape"
kill -KILL "$victim"; wait "$victim" 2>/dev/null || true
start=$(date +%s)
check "the next builder runs the same step at once" bash -c "$(printf '%q ' "$driver") step '$store' '$sources' 5 | grep -q '^ran '"
check "without waiting for the dead one" test $(( $(date +%s) - start )) -lt 15
check "no work or claims are left" test -z "$(ls -A "$store/work")"
shape=$shape0

echo "== a stale claim whose process number a live process now has"
sleep 120 &
reused=$!; pids+=("$reused")
echo third >"$sources/in.txt"
mkdir -p "$store/work/step-$shape"; echo leftover >"$store/work/step-$shape/junk"
printf '%s\nlocked\n' "$reused" >"$store/work/step-$shape.owner"
check "a claim written under a lock is stale whatever process has its number" \
    bash -c "$(printf '%q ' "$driver") step '$store' '$sources' 0 | grep -q '^ran '"
check "the dead run's directory is gone" test ! -e "$store/work/step-$shape"

echo "== a claim of an older ccl-build (no lock) naming a live process"
echo fourth >"$sources/in.txt"
mkdir -p "$store/work/step-$shape"
printf '%s\n' "$reused" >"$store/work/step-$shape.owner"
check "is respected: the step is refused" fails step 0
check "the refusal names the older build" bash -c "$(printf '%q ' "$driver") step '$store' '$sources' 0 2>&1 | grep -q 'running in an older ccl-build'"
kill "$reused"; wait "$reused" 2>/dev/null || true
check "once that process is gone the step runs" bash -c "$(printf '%q ' "$driver") step '$store' '$sources' 0 | grep -q '^ran '"

echo "== staging of dead and live processes"
sleep 120 &
alive=$!; pids+=("$alive")
mkdir -p "$store/work/$alive-7" "$store/work/999999999-1" "$store/work/not-a-process"
step 0 >/dev/null
check "a live process's staging is kept" test -d "$store/work/$alive-7"
check "a dead process's staging is removed" test ! -e "$store/work/999999999-1"
check "a malformed name is removed" test ! -e "$store/work/not-a-process"
kill "$alive"; wait "$alive" 2>/dev/null || true
step 0 >/dev/null
check "and removed once the process is gone" test ! -e "$store/work/$alive-7"
check "the store is consistent" "$driver" check "$store" complete

echo "liveness tests: $passed passed, $failed failed"
[[ $failed == 0 ]]
