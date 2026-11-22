#!/usr/bin/env bash
#  Drivers end on Stop (PWR-003): boots a shutdown test RUNS times in QEMU
#  and checks devmgr's driver stage of each (check_driver_stops.py): none
#  forced, each within the checker's Maximum_Stop_Ms, the report before S5.
#  Every run's serial log is kept under its own name, so a failure can be
#  read afterwards (a lost tail told apart from a misordered one).
#
#  Usage, from the repository root, inside nix develop, holding the build
#  lock (it boots QEMU):
#    tests/driver-stop/run.sh [--test shutdown|acpi-shutdown] [--runs N] [--out DIR]
#  shutdown (the default) has the stand-in firmware step, which lets the
#  console drain before S5, so the whole stage is in the log; acpi-shutdown
#  runs the real acpi.svc, whose S5 can cut the log's tail (LOG-009): it is
#  checked with --lossy-tail.
set -uo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
test_name=shutdown
runs=3
out="${TMPDIR:-/tmp}/driver-stop-$(date +%Y%m%d-%H%M%S)-$$"
while [ "$#" -gt 0 ]; do
    case "$1" in
        --test) test_name="$2"; shift 2 ;;
        --runs) runs="$2"; shift 2 ;;
        --out) out="$2"; shift 2 ;;
        *) echo "driver-stop: unknown argument $1" >&2; exit 2 ;;
    esac
done
case "$test_name" in
    shutdown) lossy=() ;;
    acpi-shutdown) lossy=(--lossy-tail) ;;
    *) echo "driver-stop: --test is shutdown or acpi-shutdown" >&2; exit 2 ;;
esac
mkdir -p "$out"
failed=0
for ((run = 1; run <= runs; run++)); do
    serial="$out/$test_name-serial-$run.log"
    bash "$root/tests/headless/run.sh" --test "$test_name" --accel kvm --timeout 300 \
        --serial "$serial" > "$out/$test_name-run-$run.log" 2>&1
    status=$?
    echo "driver-stop: $test_name run $run: headless exit $status ($(grep -a -E 'headless: (PASS|FAIL)' "$out/$test_name-run-$run.log" | tail -n 1))"
    if ! python3 -I "$here/check_driver_stops.py" "${lossy[@]}" "$serial" > "$out/$test_name-check-$run.log" 2>&1; then
        failed=1
    fi
    sed 's/^/  /' "$out/$test_name-check-$run.log"
    [ "$status" -eq 0 ] || failed=1
done
echo "driver-stop: logs kept in $out"
if [ "$failed" -ne 0 ]; then
    echo "driver-stop: FAIL"
    exit 1
fi
echo "driver-stop: PASS ($runs runs)"
