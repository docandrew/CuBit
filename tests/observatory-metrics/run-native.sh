#!/usr/bin/env bash
# Run in Nix under coordination/build.lock. Reuses existing staged desktop,
# metrics service and boot prerequisites; never writes a user's base disk.
set -euo pipefail
cd "$(dirname "$0")/../.."
evidence=$(mktemp -d -t cubit-observatory-native.XXXXXX)
printf 'ARTIFACTS: %s\n' "$evidence"
bash tests/observatory-metrics/build-native-check.sh
staged=kernel/isodir/boot/desktop-metrics-observer.app
had_observer=0
if [ -f "$staged" ]; then
    cp "$staged" "$evidence/previous-observer.app"
    had_observer=1
fi
restore() {
    if [ "$had_observer" = 1 ]; then
        cp "$evidence/previous-observer.app" "$staged"
    else
        rm -f "$staged"
    fi
}
trap restore EXIT
cp tests/observatory-metrics/native-check/build/observatory-check.app "$staged"
sha256sum userspace/lib/observatory/*.ad? userspace/ccl/src/*.ad? \
    userspace/lib/compositor/compositor_requests.ad? \
    tests/observatory-metrics/native-check/main.adb "$staged" \
    userspace/services/desktop/build-metrics/desktop.svc \
    kernel/isodir/boot/metrics.svc > "$evidence/inputs.sha256"
python3 tools/build_development_disk.py "$evidence/base.img" --boot \
    kernel/isodir/boot/logstore.svc kernel/isodir/boot/clock.svc \
    kernel/isodir/boot/config-storage.svc kernel/isodir/boot/tls.svc \
    --file init.ccl=tests/compositor/init-metrics-desktop.ccl
sha256sum "$evidence/base.img" > "$evidence/base.sha256"
CUBIT_DESKTOP_METRICS_TEST=1 \
CUBIT_DESKTOP_IMAGE="$PWD/userspace/services/desktop/build-metrics/desktop.svc" \
    bash tests/headless/run.sh --test ccl-workspace --disk "$evidence/base.img" \
    --accel tcg,thread=multi --cpus 4 --timeout 120 \
    --serial "$evidence/serial.log" --keep-logs
grep -F 'TEST: PASS observatory-ccl live summary expressions' "$evidence/serial.log"
grep -F 'TEST: PASS desktop stage metrics all four live' "$evidence/serial.log"
grep -F 'TEST: PASS observatory-async queries=' "$evidence/serial.log"
sha256sum -c "$evidence/inputs.sha256"
sha256sum -c "$evidence/base.sha256"
